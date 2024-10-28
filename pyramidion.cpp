#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Analysis/CGSCCPassManager.h"
#include "llvm/Analysis/LoopAnalysisManager.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Pass.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/OptimizationLevel.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>
#include <iostream>
#include <sstream>

using namespace llvm;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

enum Token {
  tok_eof = -1,
  tok_number = -2,
  tok_operator = -3,
  tok_identifier = -4,
  tok_pyramid = -5,
  tok_lparen = -6,
  tok_rparen = -7,
  tok_comma = -8,
  tok_unknown = -9
};

static double NumVal;             // Filled in if tok_number
static char OperatorChar;         // Filled in if tok_operator
static std::string IdentifierStr; // Filled in if tok_identifier

/// Trim - Utility function to trim whitespace from a string.
static std::string Trim(const std::string &str) {
  size_t first = str.find_first_not_of(" \t");
  if (first == std::string::npos) return ""; // Empty or all spaces
  size_t last = str.find_last_not_of(" \t");
  return str.substr(first, (last - first + 1));
}

/// gettok - Return the next token from standard input.
static int gettok(const std::string &line, size_t &index) {
  while (index < line.length() && isspace(line[index]))
    ++index;

  if (index >= line.length())
    return tok_eof;

  if (isalpha(line[index])) { // Identifier: [a-zA-Z][a-zA-Z0-9_]*
    IdentifierStr = "";
    while (index < line.length() && (isalnum(line[index]) || line[index] == '_'))
      IdentifierStr += line[index++];

    if (IdentifierStr == "pyramid")
      return tok_pyramid;

    return tok_identifier;
  }

  if (isdigit(line[index]) || line[index] == '.') { // Number: [0-9.]+
    std::string NumStr;

    do {
      NumStr += line[index];
      ++index;
    } while (index < line.length() && (isdigit(line[index]) || line[index] == '.'));

    // Validate the number format
    if (NumStr.find("..") != std::string::npos) {
      return tok_unknown; // Invalid number format
    }

    NumVal = strtod(NumStr.c_str(), nullptr);
    return tok_number;
  }

  if (line[index] == '+' || line[index] == '-' || line[index] == '*' || line[index] == '/') { // Operator: [+-*/]
    OperatorChar = line[index];
    ++index;
    return tok_operator;
  }

  if (line[index] == '(') {
    ++index;
    return tok_lparen;
  }
  if (line[index] == ')') {
    ++index;
    return tok_rparen;
  }
  if (line[index] == ',') {
    ++index;
    return tok_comma;
  }

  // Unknown character
  ++index;
  return tok_unknown;
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree
//===----------------------------------------------------------------------===//

namespace {

/// ExprAST - Base class for all expression nodes.
class ExprAST {
public:
  virtual ~ExprAST() = default;
  virtual void print(int depth = 0) const = 0;
  virtual Value *codegen() = 0;
};

/// NumberExprAST - Expression class for numeric literals.
class NumberExprAST : public ExprAST {
  double Val;

public:
  NumberExprAST(double Val) : Val(Val) {}
  void print(int depth = 0) const override {
    for (int i = 0; i < depth; ++i)
      std::cout << "  ";
    std::cout << "Number: " << Val << "\n";
  }

  Value *codegen() override;
};

/// VariableExprAST - Expression class for referencing a variable.
class VariableExprAST : public ExprAST {
  std::string Name;

public:
  VariableExprAST(const std::string &Name) : Name(Name) {}

  const std::string &getName() const { return Name; }

  void print(int depth = 0) const override {
    for (int i = 0; i < depth; ++i)
      std::cout << "  ";
    std::cout << "Variable: " << Name << "\n";
  }

  Value *codegen() override;
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
  char Op;
  std::shared_ptr<ExprAST> LHS, RHS;

public:
  BinaryExprAST(char Op) : Op(Op), LHS(nullptr), RHS(nullptr) {}

  void setOperands(std::shared_ptr<ExprAST> L, std::shared_ptr<ExprAST> R) {
    LHS = L;
    RHS = R;
  }

  void print(int depth = 0) const override {
    for (int i = 0; i < depth; ++i)
      std::cout << "  ";
    std::cout << "Operator: " << Op << "\n";
    if (LHS)
      LHS->print(depth + 1);
    if (RHS)
      RHS->print(depth + 1);
  }

  Value *codegen() override;
};

/// CallExprAST - Expression class for function calls.
class FunctionCallExprAST : public ExprAST {
  std::string Callee;
  std::vector<std::shared_ptr<ExprAST>> Args;

public:
  FunctionCallExprAST(const std::string &Callee) : Callee(Callee) {}

  void setOperands(const std::vector<std::shared_ptr<ExprAST>> &Operands) {
    Args = Operands;
  }

  const std::vector<std::shared_ptr<ExprAST>> &getArgs() const { return Args; }

  void print(int depth = 0) const override {
    for (int i = 0; i < depth; ++i)
      std::cout << "  ";
    std::cout << "Function Call: " << Callee << "\n";
    for (const auto &Arg : Args)
      Arg->print(depth + 1);
  }

  Value *codegen() override;
};

/// PrototypeAST - This class represents the "prototype" for a function,
class PrototypeAST {
  std::string Name;
  std::vector<std::string> Args;

public:
  PrototypeAST(const std::string &Name, std::vector<std::string> Args)
      : Name(Name), Args(std::move(Args)) {}

  Function *codegen();
  const std::string &getName() const { return Name; }
  const std::vector<std::string> &getArgs() const { return Args; }
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
  std::unique_ptr<PrototypeAST> Proto;
  std::shared_ptr<ExprAST> Body;

public:
  FunctionAST(std::unique_ptr<PrototypeAST> Proto,
              std::shared_ptr<ExprAST> Body)
      : Proto(std::move(Proto)), Body(std::move(Body)) {}

  Function *codegen();
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<IRBuilder<>> Builder;
static std::map<std::string, Value *> NamedValues;
static std::unique_ptr<legacy::FunctionPassManager> TheFPM;

std::shared_ptr<ExprAST> LogError(const char *Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
  LogError(Str);
  return nullptr;
}

Value *LogErrorV(const char *Str) {
  LogError(Str);
  return nullptr;
}

Value *NumberExprAST::codegen() {
  return ConstantFP::get(*TheContext, APFloat(Val));
}

Value *VariableExprAST::codegen() {
  Value *V = NamedValues[Name];
  if (!V)
    return LogErrorV(("Unknown variable name: " + Name).c_str());
  return V;
}

Value *BinaryExprAST::codegen() {
  Value *L = LHS->codegen();
  Value *R = RHS->codegen();
  if (!L || !R)
    return nullptr;

  switch (Op) {
  case '+':
    return Builder->CreateFAdd(L, R, "addtmp");
  case '-':
    return Builder->CreateFSub(L, R, "subtmp");
  case '*':
    return Builder->CreateFMul(L, R, "multmp");
  case '/':
    return Builder->CreateFDiv(L, R, "divtmp");
  default:
    return LogErrorV("invalid binary operator");
  }
}

Value *FunctionCallExprAST::codegen() {
  Function *CalleeF = TheModule->getFunction(Callee);
  if (!CalleeF)
    return LogErrorV(("Unknown function referenced: " + Callee).c_str());

  if (CalleeF->arg_size() != Args.size())
    return LogErrorV("Incorrect number of arguments passed");

  std::vector<Value *> ArgsV;
  for (auto &Arg : Args) {
    Value *ArgV = Arg->codegen();
    if (!ArgV)
      return nullptr;
    ArgsV.push_back(ArgV);
  }

  return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

Function *PrototypeAST::codegen() {
  std::vector<Type *> Doubles(Args.size(), Type::getDoubleTy(*TheContext));
  FunctionType *FT = FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);
  Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

  unsigned Idx = 0;
  for (auto &Arg : F->args())
    Arg.setName(Args[Idx++]);

  return F;
}

Function *FunctionAST::codegen() {
  Function *TheFunction = TheModule->getFunction(Proto->getName());
  if (!TheFunction)
    TheFunction = Proto->codegen();

  if (!TheFunction)
    return nullptr;

  if (!TheFunction->empty()) {
    LogErrorV("Function cannot be redefined.");
    return nullptr;
  }

  BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
  Builder->SetInsertPoint(BB);

  NamedValues.clear();
  for (auto &Arg : TheFunction->args())
    NamedValues[std::string(Arg.getName())] = &Arg;

  if (Value *RetVal = Body->codegen()) {
    Builder->CreateRet(RetVal);
    verifyFunction(*TheFunction, &errs());

    TheFPM->run(*TheFunction);

    return TheFunction;
  }

  TheFunction->eraseFromParent();
  return nullptr;
}

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

std::shared_ptr<ExprAST> BuildAST(const std::vector<std::vector<std::shared_ptr<ExprAST>>> &ASTLevels, size_t &levelIndex, size_t &nodeIndex) {
  if (levelIndex >= ASTLevels.size() || nodeIndex >= ASTLevels[levelIndex].size())
    return nullptr;

  auto node = ASTLevels[levelIndex][nodeIndex++];

  if (auto *BinOp = dynamic_cast<BinaryExprAST*>(node.get())) {
    
    size_t nextLevel = levelIndex + 1;
    if (nextLevel >= ASTLevels.size()) {
      LogError("Error: Incomplete pyramid structure for binary operator.");
      return nullptr;
    }

    size_t leftNodeIndex = nodeIndex - 1;
    size_t rightNodeIndex = nodeIndex;

    auto LHS = BuildAST(ASTLevels, nextLevel, leftNodeIndex);
    auto RHS = BuildAST(ASTLevels, nextLevel, rightNodeIndex);

    if (!LHS || !RHS) {
      LogError("Error: Missing operands for binary operator.");
      return nullptr;
    }

    BinOp->setOperands(LHS, RHS);
    return node;
  } else if (auto *VarNode = dynamic_cast<VariableExprAST*>(node.get())) {
    // Variable or function call
    if (NamedValues.find(VarNode->getName()) == NamedValues.end()) {
      // Not a known variable, treat it as a function call
      Function *CalleeF = TheModule->getFunction(VarNode->getName());
      if (!CalleeF) {
        LogError(("Unknown function referenced: " + VarNode->getName()).c_str());
        return nullptr;
      }
      size_t nextLevel = levelIndex + 1;
      if (nextLevel >= ASTLevels.size()) {
        LogError("Error: Incomplete pyramid structure for function call.");
        return nullptr;
      }

      size_t childNodeIndex = 0;
      std::vector<std::shared_ptr<ExprAST>> Args;
      for (unsigned i = 0; i < CalleeF->arg_size(); ++i) {
        auto Arg = BuildAST(ASTLevels, nextLevel, childNodeIndex);
        if (!Arg) {
          LogError("Error: Missing arguments for function call.");
          return nullptr;
        }
        Args.push_back(Arg);
      }
      auto FuncCall = std::make_shared<FunctionCallExprAST>(VarNode->getName());
      FuncCall->setOperands(Args);
      return FuncCall;
    } else {
      // Known variable
      return node;
    }
  } else if (auto *NumNode = dynamic_cast<NumberExprAST*>(node.get())) {
    return node;
  }

  return nullptr;
}

static bool ParsePyramid(std::istream &input) {
  std::string line;
  std::vector<std::string> lines;
  std::vector<std::vector<std::shared_ptr<ExprAST>>> ASTLevels;

  // Read lines until we encounter an empty line or EOF
  while (std::getline(input, line)) {
    line = Trim(line);
    if (line.empty()) {
      break; // End of the current pyramid
    }

    lines.push_back(line); // Store the line

    std::vector<std::shared_ptr<ExprAST>> ASTNodes;
    size_t index = 0;
    while (index < line.size()) {
      int tok = gettok(line, index);

      if (tok == tok_eof)
        break;

      if (tok == tok_number) {
        ASTNodes.push_back(std::make_shared<NumberExprAST>(NumVal));
      } else if (tok == tok_operator) {
        ASTNodes.push_back(std::make_shared<BinaryExprAST>(OperatorChar));
      } else if (tok == tok_identifier) {
        ASTNodes.push_back(std::make_shared<VariableExprAST>(IdentifierStr));
      } else if (tok == tok_unknown) {
        LogError("Error: Unknown token encountered.");
        return false;
      }
    }

    ASTLevels.push_back(std::move(ASTNodes));
  }

  if (ASTLevels.empty()) {
    return false;
  }

  
  bool isFunctionDefinition = false;
  std::string funcName;
  std::vector<std::string> funcArgs;

  // Get the last line
  std::string lastLine = lines.back();
  size_t index = 0;
  int tok = gettok(lastLine, index);
  if (tok == tok_pyramid) {
    isFunctionDefinition = true;

    // Rest of the line after 'pyramid'
    std::string rest = lastLine.substr(index);
    rest = Trim(rest);

   
    size_t pos = rest.find('(');
    if (pos != std::string::npos) {
      funcName = rest.substr(0, pos);
      funcName = Trim(funcName);

      std::string argsStr = rest.substr(pos + 1);
      // Remove closing ')'
      if (!argsStr.empty() && argsStr.back() == ')')
        argsStr.pop_back();

      // Split arguments by ','
      std::istringstream argsStream(argsStr);
      std::string arg;
      while (std::getline(argsStream, arg, ',')) {
        // Trim whitespace
        arg = Trim(arg);
        funcArgs.push_back(arg);
      }
    } else {
      LogError("Error: Invalid function definition.");
      return true;
    }

    // Remove the last level as it's the function definition
    ASTLevels.pop_back();
    lines.pop_back();
  }

  // Set up known variables
  if (isFunctionDefinition) {
    NamedValues.clear();
    for (const auto &arg : funcArgs) {
      NamedValues[arg] = nullptr;
    }
  } else {
    NamedValues.clear();
  }

  // Build the AST recursively
  std::shared_ptr<ExprAST> Root;
  size_t levelIndex = 0;
  size_t nodeIndex = 0;
  Root = BuildAST(ASTLevels, levelIndex, nodeIndex);

  if (!Root) {
    LogError("Error: Failed to build AST.");
    return true;
  }

  if (isFunctionDefinition) {
    auto Proto = std::make_unique<PrototypeAST>(funcName, funcArgs);
    auto FnAST = std::make_unique<FunctionAST>(std::move(Proto), Root);

    if (auto *FnIR = FnAST->codegen()) {
      FnIR->print(errs());
    }
  } else {
    // Generate a 'main' function to evaluate the expression
    auto Proto = std::make_unique<PrototypeAST>("main", std::vector<std::string>());
    auto FnAST = std::make_unique<FunctionAST>(std::move(Proto), Root);

    if (auto *FnIR = FnAST->codegen()) {
      FnIR->print(errs());
    }
  }

  return true;
}

//===----------------------------------------------------------------------===//
// Top-Level parsing
//===----------------------------------------------------------------------===//

void InitializeModule() {
  TheContext = std::make_unique<LLVMContext>();
  TheModule = std::make_unique<Module>("Pyramidion JIT", *TheContext);
  Builder = std::make_unique<IRBuilder<>>(*TheContext);

  // Create the pass manager
  TheFPM = std::make_unique<legacy::FunctionPassManager>(TheModule.get());

  // Add some optimizations
  TheFPM->add(createInstructionCombiningPass());
  TheFPM->add(createReassociatePass());
  TheFPM->add(createGVNPass());
  TheFPM->add(createCFGSimplificationPass());

  TheFPM->doInitialization();
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main(int argc, char **argv) {

  InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();
  InitializeNativeTargetAsmParser();

  if (argc < 2) {
    std::cerr << "Usage: " << argv[0] << " <input file>\n";
    return 1;
  }

  std::ifstream inputFile(argv[1]);
  if (!inputFile) {
    std::cerr << "Error: Cannot open file " << argv[1] << "\n";
    return 1;
  }

  InitializeModule();

  // Parse pyramids
  while (true) {
    bool parsed = ParsePyramid(inputFile);
    if (!parsed)
      break;
  }

  // Verify the module
  if (verifyModule(*TheModule, &errs())) {
    errs() << "Error: module verification failed\n";
    return 1;
  }

  // Set up the ExecutionEngine
  std::string ErrStr;
  ExecutionEngine *EE = EngineBuilder(std::move(TheModule))
                            .setErrorStr(&ErrStr)
                            .setEngineKind(EngineKind::JIT)
                            .create();
  if (!EE) {
    errs() << "Failed to construct ExecutionEngine: " << ErrStr << "\n";
    return 1;
  }

  // Run the 'main' function if it exists
  Function *MainFunction = EE->FindFunctionNamed("main");
  if (MainFunction) {
    std::vector<GenericValue> Args; // Add arguments if needed
    GenericValue Result = EE->runFunction(MainFunction, Args);
    outs() << "Result: " << Result.DoubleVal << "\n";
  } else {
    errs() << "No 'main' function found in module.\n";
  }

  return 0;
}
