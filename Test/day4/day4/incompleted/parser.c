/*
 * @copyright (c) 2008, Hedspi, Hanoi University of Technology
 * @author Huu-Duc Nguyen
 * @version 1.0
 */
#include <stdio.h>
#include <stdlib.h>

#include "reader.h"
#include "scanner.h"
#include "parser.h"
#include "semantics.h"
#include "error.h"
#include "debug.h"
#include "symtab.h"

Token *currentToken;
Token *lookAhead;

extern Type* intType;
extern Type* charType;
extern SymTab* symtab;

void scan(void) {
  Token* tmp = currentToken;
  currentToken = lookAhead;
  lookAhead = getValidToken();
  free(tmp);
}

void eat(TokenType tokenType) {
  if (lookAhead->tokenType == tokenType) {
    // printf("Consuming token: %d\n", tokenType);
    scan();
  } else missingToken(tokenType, lookAhead->lineNo, lookAhead->colNo);
}

void compileProgram(void) {
  Object* program;

  eat(KW_PROGRAM);
  eat(TK_IDENT);

  program = createProgramObject(currentToken->string);
  enterBlock(program->progAttrs->scope);

  eat(SB_SEMICOLON);

  compileBlock();
  eat(SB_PERIOD);

  exitBlock();
}

void compileBlock(void) {
  Object* constObj;
  ConstantValue* constValue;

  if (lookAhead->tokenType == KW_CONST) {
    eat(KW_CONST);

    do {
      eat(TK_IDENT);

      checkFreshIdent(currentToken->string);
      constObj = createConstantObject(currentToken->string);

      eat(SB_EQ);
      constValue = compileConstant();

      constObj->constAttrs->value = constValue;
      declareObject(constObj);

      eat(SB_SEMICOLON);
    } while (lookAhead->tokenType == TK_IDENT);

    compileBlock2();
  }
  else compileBlock2();
}

void compileBlock2(void) {
  Object* typeObj;
  Type* actualType;

  if (lookAhead->tokenType == KW_TYPE) {
    eat(KW_TYPE);

    do {
      eat(TK_IDENT);

      checkFreshIdent(currentToken->string);
      typeObj = createTypeObject(currentToken->string);

      eat(SB_EQ);
      actualType = compileType();

      typeObj->typeAttrs->actualType = actualType;
      declareObject(typeObj);

      eat(SB_SEMICOLON);
    } while (lookAhead->tokenType == TK_IDENT);

    compileBlock3();
  }
  else compileBlock3();
}

void compileBlock3(void) {
  Object* varObj;
  Type* varType;

  if (lookAhead->tokenType == KW_VAR) {
    eat(KW_VAR);

    do {
      eat(TK_IDENT);

      checkFreshIdent(currentToken->string);
      varObj = createVariableObject(currentToken->string);

      eat(SB_COLON);
      varType = compileType();

      varObj->varAttrs->type = varType;
      declareObject(varObj);

      eat(SB_SEMICOLON);
    } while (lookAhead->tokenType == TK_IDENT);

    compileBlock4();
  }
  else compileBlock4();
}

void compileBlock4(void) {
  compileSubDecls();
  compileBlock5();
}

void compileBlock5(void) {
  eat(KW_BEGIN);
  compileStatements();
  eat(KW_END);
}

void compileSubDecls(void) {
  while ((lookAhead->tokenType == KW_FUNCTION) || (lookAhead->tokenType == KW_PROCEDURE)) {
    if (lookAhead->tokenType == KW_FUNCTION)
      compileFuncDecl();
    else compileProcDecl();
  }
}

void compileFuncDecl(void) {
  Object* funcObj;
  Type* returnType;

  eat(KW_FUNCTION);
  eat(TK_IDENT);

  checkFreshIdent(currentToken->string);
  funcObj = createFunctionObject(currentToken->string);
  declareObject(funcObj);

  enterBlock(funcObj->funcAttrs->scope);

  compileParams();

  eat(SB_COLON);
  returnType = compileBasicType();
  funcObj->funcAttrs->returnType = returnType;

  eat(SB_SEMICOLON);
  compileBlock();
  eat(SB_SEMICOLON);

  exitBlock();
}

void compileProcDecl(void) {
  Object* procObj;

  eat(KW_PROCEDURE);
  eat(TK_IDENT);

  checkFreshIdent(currentToken->string);
  procObj = createProcedureObject(currentToken->string);
  declareObject(procObj);

  enterBlock(procObj->procAttrs->scope);

  compileParams();

  eat(SB_SEMICOLON);
  compileBlock();
  eat(SB_SEMICOLON);

  exitBlock();
}

ConstantValue* compileUnsignedConstant(void) {
  ConstantValue* constValue;
  Object* obj;

  switch (lookAhead->tokenType) {
  case TK_NUMBER:
    eat(TK_NUMBER);
    constValue = makeIntConstant(currentToken->value);
    break;
  case TK_IDENT:
    eat(TK_IDENT);

    obj = checkDeclaredConstant(currentToken->string);
    constValue = duplicateConstantValue(obj->constAttrs->value);

    break;
  case TK_CHAR:
    eat(TK_CHAR);
    constValue = makeCharConstant(currentToken->string[0]);
    break;
  default:
    error(ERR_INVALID_CONSTANT, lookAhead->lineNo, lookAhead->colNo);
    break;
  }
  return constValue;
}

ConstantValue* compileConstant(void) {
  ConstantValue* constValue;

  switch (lookAhead->tokenType) {
  case SB_PLUS:
    eat(SB_PLUS);
    constValue = compileConstant2();
    break;
  case SB_MINUS:
    eat(SB_MINUS);
    constValue = compileConstant2();
    constValue->intValue = - constValue->intValue;
    break;
  case TK_CHAR:
    eat(TK_CHAR);
    constValue = makeCharConstant(currentToken->string[0]);
    break;
  default:
    constValue = compileConstant2();
    break;
  }
  return constValue;
}

ConstantValue* compileConstant2(void) {
  ConstantValue* constValue;
  Object* obj;

  switch (lookAhead->tokenType) {
  case TK_NUMBER:
    eat(TK_NUMBER);
    constValue = makeIntConstant(currentToken->value);
    break;
  case TK_IDENT:
    eat(TK_IDENT);
    obj = checkDeclaredConstant(currentToken->string);
    if (obj->constAttrs->value->type == TP_INT)
      constValue = duplicateConstantValue(obj->constAttrs->value);
    else
      error(ERR_UNDECLARED_INT_CONSTANT,currentToken->lineNo, currentToken->colNo);
    break;
  default:
    error(ERR_INVALID_CONSTANT, lookAhead->lineNo, lookAhead->colNo);
    break;
  }
  return constValue;
}

Type* compileType(void) {
  Type* type;
  Type* elementType;
  int arraySize;
  Object* obj;

  switch (lookAhead->tokenType) {
  case KW_INTEGER:
    eat(KW_INTEGER);
    type =  makeIntType();
    break;
  case KW_CHAR:
    eat(KW_CHAR);
    type = makeCharType();
    break;
  case KW_ARRAY:
    eat(KW_ARRAY);
    eat(SB_LSEL);
    eat(TK_NUMBER);

    arraySize = currentToken->value;

    eat(SB_RSEL);
    eat(KW_OF);
    elementType = compileType();
    type = makeArrayType(arraySize, elementType);
    break;
  case TK_IDENT:
    eat(TK_IDENT);
    obj = checkDeclaredType(currentToken->string);
    type = duplicateType(obj->typeAttrs->actualType);
    break;
  default:
    error(ERR_INVALID_TYPE, lookAhead->lineNo, lookAhead->colNo);
    break;
  }
  return type;
}

Type* compileBasicType(void) {
  Type* type;

  switch (lookAhead->tokenType) {
  case KW_INTEGER:
    eat(KW_INTEGER);
    type = makeIntType();
    break;
  case KW_CHAR:
    eat(KW_CHAR);
    type = makeCharType();
    break;
  default:
    error(ERR_INVALID_BASICTYPE, lookAhead->lineNo, lookAhead->colNo);
    break;
  }
  return type;
}

void compileParams(void) {
  if (lookAhead->tokenType == SB_LPAR) {
    eat(SB_LPAR);
    compileParam();
    while (lookAhead->tokenType == SB_SEMICOLON) {
      eat(SB_SEMICOLON);
      compileParam();
    }
    eat(SB_RPAR);
  }
}

void compileParam(void) {
  Object* param;
  Type* type;
  enum ParamKind paramKind;

  switch (lookAhead->tokenType) {
  case TK_IDENT:
    paramKind = PARAM_VALUE;
    break;
  case KW_VAR:
    eat(KW_VAR);
    paramKind = PARAM_REFERENCE;
    break;
  default:
    error(ERR_INVALID_PARAMETER, lookAhead->lineNo, lookAhead->colNo);
    break;
  }

  eat(TK_IDENT);
  checkFreshIdent(currentToken->string);
  param = createParameterObject(currentToken->string, paramKind, symtab->currentScope->owner);
  eat(SB_COLON);
  type = compileBasicType();
  param->paramAttrs->type = type;
  declareObject(param);
}

void compileStatements(void) {
  compileStatement();
  while (lookAhead->tokenType == SB_SEMICOLON) {
    eat(SB_SEMICOLON);
    compileStatement();
  }
}

void compileStatement(void) {
  Token* nextToken = peekNextToken();
  switch (lookAhead->tokenType) {
  case TK_IDENT:
    if (nextToken->tokenType == SB_COMMA) {
      compileMultiAssignSt();
    } else {
      // printf("Parsing assignment statement\n");
      compileAssignSt();
    }
    break;
  case KW_CALL:
    compileCallSt();
    break;
  case KW_BEGIN:
    compileGroupSt();
    break;
  case KW_IF:
    compileIfSt();
    break;
  case KW_WHILE:
    compileWhileSt();
    break;
  case KW_FOR:
    compileForSt();
    break;
    // EmptySt needs to check FOLLOW tokens
  case SB_SEMICOLON:
  case KW_END:
  case KW_ELSE:
    break;
    // Error occurs
  default:
    error(ERR_INVALID_STATEMENT, lookAhead->lineNo, lookAhead->colNo);
    break;
  }
}

Type* compileLValue(void) {
  // Parse a lvalue (a variable, an array element, a parameter, the current function identifier)
  Object* var;
  Type* varType = NULL; // Initialize varType

  eat(TK_IDENT);
  // Check if the identifier is a function identifier, or a variable identifier, or a parameter
  var = checkDeclaredLValueIdent(currentToken->string);
  switch (var->kind) {
    case OBJ_VARIABLE:
      varType = compileIndexes(var->varAttrs->type);
      break;
    case OBJ_PARAMETER:
      varType = var->paramAttrs->type;
      break;
    case OBJ_FUNCTION:
      varType = var->funcAttrs->returnType;
      break;
    default:
      error(ERR_INVALID_IDENT, currentToken->lineNo, currentToken->colNo);
      break;
  }

  return varType;
}

void compileAssignSt(void) {
  // Parse the assignment and check type consistency
  Type* lvalueType = compileLValue();
  // printf("Parsing %d\n", lvalueType->typeClass);
  // printf("Parsing %d\n", lookAhead->tokenType);
  eat(SB_ASSIGN);
  Type* exprType = compileExpression();

  // Check type consistency
  checkTypeEquality(lvalueType, exprType);
}

void compileMultiAssignSt(void) {
  ObjectNode* varList = NULL;
  ObjectNode* exprList = NULL;
  Object* var;
  Type* varType;
  Type* exprType;

  // Parse the list of variables
  do {
    eat(TK_IDENT);
    var = checkDeclaredLValueIdent(currentToken->string);
    addObject(&varList, var);
    if (lookAhead->tokenType == SB_COMMA) {
      eat(SB_COMMA);
    } else {
      break;
    }
  } while (lookAhead->tokenType == TK_IDENT);

  eat(SB_ASSIGN);

  // Parse the list of expressions
  do {
    exprType = compileExpression();
    addObject(&exprList, createVariableObject("")); // Dummy object to hold the type
    exprList->object->varAttrs->type = exprType;
    if (lookAhead->tokenType == SB_COMMA) {
      eat(SB_COMMA);
    } else {
      break;
    }
  } while (lookAhead->tokenType == TK_IDENT || lookAhead->tokenType == TK_NUMBER || lookAhead->tokenType == TK_CHAR);

  // Check the number of variables and expressions
  ObjectNode* varNode = varList;
  ObjectNode* exprNode = exprList;
  while (varNode != NULL && exprNode != NULL) {
    varType = varNode->object->varAttrs->type;
    exprType = exprNode->object->varAttrs->type;
    checkTypeEquality(varType, exprType);
    varNode = varNode->next;
    exprNode = exprNode->next;
  }

  if (varNode != NULL || exprNode != NULL) {
    error(ERR_PARAMETERS_ARGUMENTS_INCONSISTENCY, lookAhead->lineNo, lookAhead->colNo);
  }

  // Free the temporary lists
  freeObjectList(varList);
  freeObjectList(exprList);
}

void compileCallSt(void) {
  Object* proc;

  eat(KW_CALL);
  eat(TK_IDENT);

  proc = checkDeclaredProcedure(currentToken->string);

  compileArguments(proc->procAttrs->paramList);
}

void compileGroupSt(void) {
  eat(KW_BEGIN);
  compileStatements();
  eat(KW_END);
}

void compileIfSt(void) {
  eat(KW_IF);
  compileCondition();
  eat(KW_THEN);
  compileStatement();
  if (lookAhead->tokenType == KW_ELSE)
    compileElseSt();
}

void compileElseSt(void) {
  eat(KW_ELSE);
  compileStatement();
}

void compileWhileSt(void) {
  eat(KW_WHILE);
  compileCondition();
  eat(KW_DO);
  compileStatement();
}

void compileForSt(void) {
  // Check type consistency of FOR's variable
  eat(KW_FOR);
  eat(TK_IDENT);

  // Check if the identifier is a variable
  Object* var = checkDeclaredVariable(currentToken->string);
  Type* varType = var->varAttrs->type;

  eat(SB_ASSIGN);
  Type* initExprType = compileExpression();

  // Check type consistency of the initial expression
  checkTypeEquality(varType, initExprType);

  eat(KW_TO);
  Type* finalExprType = compileExpression();

  // Check type consistency of the final expression
  checkTypeEquality(varType, finalExprType);

  eat(KW_DO);
  compileStatement();
}

void compileArgument(Object* param) {
  // Parse an argument, and check type consistency
  Type* argType;

  if (param->paramAttrs->kind == PARAM_REFERENCE) {
    // If the parameter is a reference, the argument must be a lvalue
    argType = compileLValue();
  } else {
    argType = compileExpression();
  }

  // Check type consistency
  checkTypeEquality(param->paramAttrs->type, argType);
}

void compileArguments(ObjectNode* paramList) {
  // Parse a list of arguments, check the consistency of the arguments and the given parameters
  switch (lookAhead->tokenType) {
  case SB_LPAR:
    eat(SB_LPAR);
    compileArgument(paramList->object);
    paramList = paramList->next;

    while (lookAhead->tokenType == SB_COMMA) {
      eat(SB_COMMA);
      if (paramList == NULL) {
        error(ERR_PARAMETERS_ARGUMENTS_INCONSISTENCY, currentToken->lineNo, currentToken->colNo);
      }
      compileArgument(paramList->object);
      paramList = paramList->next;
    }

    eat(SB_RPAR);
    if (paramList != NULL) {
      error(ERR_PARAMETERS_ARGUMENTS_INCONSISTENCY, currentToken->lineNo, currentToken->colNo);
    }
    break;
    // Check FOLLOW set
  case SB_TIMES:
  case SB_SLASH:
  case SB_PLUS:
  case SB_MINUS:
  case KW_TO:
  case KW_DO:
  case SB_RPAR:
  case SB_COMMA:
  case SB_EQ:
  case SB_NEQ:
  case SB_LE:
  case SB_LT:
  case SB_GE:
  case SB_GT:
  case SB_RSEL:
  case SB_SEMICOLON:
  case KW_END:
  case KW_ELSE:
  case KW_THEN:
    break;
  default:
    error(ERR_INVALID_ARGUMENTS, lookAhead->lineNo, lookAhead->colNo);
  }
}

void compileCondition(void) {
  // Check the type consistency of LHS and RHS, check the basic type
  Type* lhsType = compileExpression();

  switch (lookAhead->tokenType) {
  case SB_EQ:
    eat(SB_EQ);
    break;
  case SB_NEQ:
    eat(SB_NEQ);
    break;
  case SB_LE:
    eat(SB_LE);
    break;
  case SB_LT:
    eat(SB_LT);
    break;
  case SB_GE:
    eat(SB_GE);
    break;
  case SB_GT:
    eat(SB_GT);
    break;
  default:
    error(ERR_INVALID_COMPARATOR, lookAhead->lineNo, lookAhead->colNo);
  }

  Type* rhsType = compileExpression();

  // Check type consistency
  checkTypeEquality(lhsType, rhsType);

  // Check if both LHS and RHS are of basic types
  checkBasicType(lhsType);
  checkBasicType(rhsType);
}

Type* compileExpression(void) {
  Type* type;

  switch (lookAhead->tokenType) {
  case SB_PLUS:
    eat(SB_PLUS);
    type = compileExpression2();
    checkIntType(type);
    break;
  case SB_MINUS:
    eat(SB_MINUS);
    type = compileExpression2();
    checkIntType(type);
    break;
  default:
    type = compileExpression2();
  }
  return type;
}

Type* compileExpression2(void) {
  Type* type1;
  Type* type2;

  type1 = compileTerm();
  type2 = compileExpression3();
  if (type2 == NULL) return type1;
  else {
    checkTypeEquality(type1,type2);
    return type1;
  }
}

Type* compileExpression3(void) {
  Type* type1;
  Type* type2;

  switch (lookAhead->tokenType) {
  case SB_PLUS:
    eat(SB_PLUS);
    type1 = compileTerm();
    checkIntType(type1);
    type2 = compileExpression3();
    if (type2 != NULL)
      checkIntType(type2);
    return type1;
  case SB_MINUS:
    eat(SB_MINUS);
    type1 = compileTerm();
    checkIntType(type1);
    type2 = compileExpression3();
    if (type2 != NULL)
      checkIntType(type2);
    return type1;
    // check the FOLLOW set
  case KW_TO:
  case KW_DO:
  case SB_RPAR:
  case SB_COMMA:
  case SB_EQ:
  case SB_NEQ:
  case SB_LE:
  case SB_LT:
  case SB_GE:
  case SB_GT:
  case SB_RSEL:
  case SB_SEMICOLON:
  case KW_END:
  case KW_ELSE:
  case KW_THEN:
    return NULL;
  default:
    error(ERR_INVALID_EXPRESSION, lookAhead->lineNo, lookAhead->colNo);
    return NULL; // Ensure a return value in case of error
  }
}

Type* compileTerm(void) {
  // Check type of Term2
  Type* type;

  type = compileFactor();
  Type* term2Type = compileTerm2();

  // Check type consistency if term2Type is not NULL
  if (term2Type != NULL) {
    checkTypeEquality(type, term2Type);
  }

  return type;
}

Type* compileTerm2(void) {
  // Check type of term2
  Type* type = NULL;

  switch (lookAhead->tokenType) {
  case SB_TIMES:
    eat(SB_TIMES);
    type = compileFactor();
    checkIntType(type);
    compileTerm2();
    break;
  case SB_SLASH:
    eat(SB_SLASH);
    type = compileFactor();
    checkIntType(type);
    compileTerm2();
    break;
    // check the FOLLOW set
  case SB_PLUS:
  case SB_MINUS:
  case KW_TO:
  case KW_DO:
  case SB_RPAR:
  case SB_COMMA:
  case SB_EQ:
  case SB_NEQ:
  case SB_LE:
  case SB_LT:
  case SB_GE:
  case SB_GT:
  case SB_RSEL:
  case SB_SEMICOLON:
  case KW_END:
  case KW_ELSE:
  case KW_THEN:
    break;
  default:
    error(ERR_INVALID_TERM, lookAhead->lineNo, lookAhead->colNo);
  }

  return type;
}

Type* compileFactor(void) {
  // Parse a factor and return the factor's type

  Object* obj;
  Type* type = NULL;

  switch (lookAhead->tokenType) {
  case TK_NUMBER:
    eat(TK_NUMBER);
    type = makeIntType();
    break;
  case TK_CHAR:
    eat(TK_CHAR);
    type = makeCharType();
    break;
  case TK_IDENT:
    eat(TK_IDENT);
    // check if the identifier is declared
    obj = checkDeclaredIdent(currentToken->string);

    switch (obj->kind) {
    case OBJ_CONSTANT:
      type = (Type*) malloc(sizeof(Type));
      type->typeClass = obj->constAttrs->value->type;
      break;
    case OBJ_VARIABLE:
      type = compileIndexes(obj->varAttrs->type);
      break;
    case OBJ_PARAMETER:
      type = obj->paramAttrs->type;
      break;
    case OBJ_FUNCTION:
      type = obj->funcAttrs->returnType;
      compileArguments(obj->funcAttrs->paramList);
      break;
    default:
      error(ERR_INVALID_FACTOR, currentToken->lineNo, currentToken->colNo);
      break;
    }
    break;
  default:
    error(ERR_INVALID_FACTOR, lookAhead->lineNo, lookAhead->colNo);
  }

  return type;
}

Type* compileIndexes(Type* arrayType) {
  // Parse a sequence of indexes, check the consistency to the arrayType, and return the element type
  Type* elementType = arrayType;

  while (lookAhead->tokenType == SB_LSEL) {
    if (elementType->typeClass != TP_ARRAY) {
      error(ERR_TYPE_INCONSISTENCY, currentToken->lineNo, currentToken->colNo);
    }
    eat(SB_LSEL);
    compileExpression();
    eat(SB_RSEL);
    elementType = elementType->elementType;
  }

  return elementType;
}

int compile(char *fileName) {
  if (openInputStream(fileName) == IO_ERROR)
    return IO_ERROR;

  currentToken = NULL;
  lookAhead = getValidToken();

  initSymTab();

  compileProgram();

  printObject(symtab->program,0);

  cleanSymTab();

  free(currentToken);
  free(lookAhead);
  closeInputStream();
  return IO_SUCCESS;

}
