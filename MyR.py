import ply.lex as lex
import ply.yacc as yacc
import graphics as gra
from Tables import *
from SemCube import *

reserved = {
    'int' : 'INT',
    'float' : 'FLOAT',
    'char' : 'CHAR',
    'if' : 'IF',
    'then' : 'THEN',
    'else' : 'ELSE',
    'while' : 'WHILE',
    'do' : 'DO',
    'get' : 'GET',
    'out' : 'OUT',
    'for' : 'FOR',
    'to' : 'TO',
    'function' : 'FUNCTION',
    'void' : 'VOID',
    'draw' : 'DRAW',
    'bool' : 'BOOL',
    'true' : 'TRUE',
    'false' : 'FALSE',
    'start' : 'START',
    'main' : 'MAIN',
    'programa' : 'PROGRAMA',
    'return' : 'RETURN'
}

tokens = [
    #Literals
    'ID', 'CTEI', 'CTEF', 'CTEC', 'CTEB',

    #Aritmetic operators
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 
    'OR', 'AND', 'LT', 'LE', 'GT', 'GE', 'EQ', 'NE',

     # Assignment (=, *=, /=, +=, -=)
    'EQUALS', 'TIMESEQUAL', 'DIVEQUAL', 'PLUSEQUAL', 'MINUSEQUAL',

     # Increment/decrement (++,--)
    'INCREMENT', 'DECREMENT',

     # Delimeters ( ) [ ] { } , . ; :
    'LPAREN', 'RPAREN',
    'LBRACKET', 'RBRACKET',
    'LBRACE', 'RBRACE',
    'COMMA', 'PERIOD', 'SEMI', 'COLON',

    # Other (...)
    'COMMENT', 'LETRERO'
] + list(reserved.values())

# Operators
t_PLUS             = r'\+'
t_MINUS            = r'-'
t_TIMES            = r'\*'
t_DIVIDE           = r'/'
t_OR               = r'\|'
t_AND              = r'&'
t_LT               = r'<'
t_GT               = r'>'
t_LE               = r'<='
t_GE               = r'>='
t_EQ               = r'=='
t_NE               = r'!='

# Assignment operators

t_EQUALS           = r'='
t_TIMESEQUAL       = r'\*='
t_DIVEQUAL         = r'/='
t_PLUSEQUAL        = r'\+='
t_MINUSEQUAL       = r'-='


# Delimeters
t_LPAREN           = r'\('
t_RPAREN           = r'\)'
t_LBRACKET         = r'\['
t_RBRACKET         = r'\]'
t_LBRACE           = r'\{'
t_RBRACE           = r'\}'
t_COMMA            = r','
t_PERIOD           = r'\.'
t_SEMI             = r';'
t_COLON            = r':'

t_ignore = ' \t'

# Identifiers
def t_ID(t):
    r'[A-Za-z_][A-Za-z0-9_]*'
    t.type = reserved.get(t.value.lower(), 'ID')
    return t

# Floating literal
def t_CTEF(t):
    r'-?\d+\.\d+'
    
    t.value = float(t.value)
    return t

# Integer literal
def t_CTEI(t):
    r'-?\d+([uU]|[lL]|[uU][lL]|[lL][uU])?'
    t.value = int(t.value)
    return t


#def t_CTEB(t):
 #   r'^(?i)(TRUE|FALSE)$'
  #  t.value = bool(t.value)
   # print("hi1")
    #return t

# Char literal
def t_CHAR(t):
    r'\'.\''
    t.value = t.value[1]
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

#Letreros
def t_LETRERO(t):
    r'"[^"]*"'
    return t

def t_COMMENT(t):
    r'\#.*'
    pass


def t_error(t): 
    print("Illegal expresion or illegal character: '%s'"% t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()

#Stacks y variables de apoyo
directory = func_Dir()
stackOper = []
stackOand = []
stackVars = []
quadruple = []
jumpStack = []
Boolstack = []
funcJump = []
funcParams = []
tablaVars = tabla_Vars()
cube = SemCube()
types = ['int', 'float', 'char', 'bool']
global_var = False
temp_count = 0
symbol_table = {}
constants = {}
temps = {}
currLine = 1
local = False
activeFunc = 'none'
funcNames = []

#Rangos de memoria virtual y sus respectivos contadores
local_int_memory = (1000, 2000)
local_float_memory = (2000, 3000)
local_char_memory = (3000, 4000)

contLocalInt = local_int_memory[0]
contLocalFloat = local_float_memory[0]
contLocalChar = local_char_memory[0]

global_int_memory = (4000, 5000)
global_float_memory = (6000, 7000)
global_char_memory = (7000, 8000)

contGlobalInt = global_int_memory[0]
contGlobalFloat = global_float_memory[0]
contGlobalChar = global_char_memory[0]

temp_int_memory = (8000, 9000)
temp_float_memory = (9000, 10000)
temp_char_memory = (10000, 11000)
temp_bool_memory = (11000, 12000)

contTempInt = temp_int_memory[0]
contTempFloat = temp_float_memory[0]
contTempBool = temp_bool_memory[0]
contTempChar = temp_char_memory[0]

const_int_memory = (12000, 13000)
const_float_memory = (12000, 13000)
const_char_memory = (12000, 13000)

#Asigna memoria a variables
def allocate_memory(scope, var_type):
    global contLocalInt, contLocalFloat, contLocalChar, contGlobalInt, contGlobalFloat, contGlobalChar

    if scope == 'local':
        if var_type == 'int':
            mem = contLocalInt
            contLocalInt += 1
        elif var_type == 'float':
            mem = contLocalFloat
            contLocalFloat += 1
        elif var_type == 'char':
            mem = contLocalChar
            contLocalChar += 1

    else:
        if var_type == 'int':
            mem = contGlobalInt
            contGlobalInt += 1
        elif var_type == 'float':
            mem = contGlobalFloat
            contGlobalFloat += 1
        elif var_type == 'char':
            mem = contGlobalChar
            contGlobalChar += 1

    return mem

#Asigna memoria a valores temporales
def allocate_tempMem(var_type):
    global contTempInt, contTempFloat, contTempBool, contTempChar 

    if var_type == 'int':
            mem = contTempInt
            contTempInt += 1
    elif var_type == 'float':
            mem = contTempFloat
            contTempFloat += 1
    elif var_type == 'char':
            mem = contTempChar
            contTempChar += 1
    elif var_type == 'bool':
            mem = contTempBool
            contTempBool += 1
    
    return mem

const_cont = 0


#Asigna memoria a arreglos
def allocate_array(name, vartype, size, scope):

    if tablaVars.exists_array(name):
        raise ValueError(f"Array {name} has already been declared")
    
    total_memory = allocate_memory(scope, vartype)

    #Asigna memoria a cada casilla del arreglo
    for i in range(size):
        element_memory = total_memory + i
        tablaVars.add_var(name + f"[{i}]", vartype, scope)
        tablaVars.set_var_mem(name + f"[{i}]", element_memory)
        tablaVars.set_var_value(name + f"[{i}]", 0)

    tablaVars.add_array(name, vartype, scope, size)
    


precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE')
)

def p_programa(p):
    '''
    programa : PROGRAMA ID SEMI bloque main
    '''
    print("Ejecutando Programa", p[2])
    quadruple.append(('ENDPROG', p[2], '', ''))

def p_main(p):
    '''
    main : MAIN bloque_use
    '''
    global currLine
    global local
    local = True
    currLine += 1
    p[0] = p[1]

def p_bloque_use(p):
    '''
    bloque_use : LBRACE bloque RBRACE
    '''

def p_bloque(p):
    '''
    bloque : estatuto bloque
           | empty

    '''
    p[0] = p[1]

def p_estatuto(p):
    '''
    estatuto : asigna
             | asignavar
             | asignaarr
             | vararr
             | var
             | func
             | ifelse
             | while
             | forloop
             | write
             | read
             | fun_esp
             | cvar
             | cfunc
    '''

def p_var(p):
    '''
    var : type ID SEMI
    '''
    global local
    vartype = p[1]
    name = p[2]
    if not local:
        scope = 'global'
    else:
        scope = 'local'
    mem = allocate_memory(scope, vartype)
    tablaVars.add_var(name, vartype, scope)
    tablaVars.set_var_mem(name, mem)
    p[0] = "Declaracion"


def p_type(p):
    '''
    type : INT
         | FLOAT
         | CHAR
    
    '''
    p[0] = p[1]

def p_asigna(p):
    '''
    asigna : ID EQUALS lit SEMI

    '''
    global currLine
    currLine += 1
    var_name = p[1]
    if not tablaVars.exists_var(var_name):
        print(f"ERROR: variable does not exist")
    else:
        value = p[3]
        tablaVars.set_var_value(var_name, value)
    p[0] = "Asignacion"


def p_asignaVar(p):
    '''
    asignavar : ID EQUALS expr SEMI
    '''
    global currLine
    currLine += 1
    var_name = p[1]
    memRight = tablaVars.get_var_mem(var_name)
    memLeft = p[3]
    if not tablaVars.exists_var(var_name):
        print(f"Error: variable does not exist")
    else:
        quadruple.append(('=', memLeft, '', memRight))

def p_literals(p):
    '''
    lit : CTEI
        | CTEF
    '''
    p[0] = p[1]

def p_write(p):
    '''
    write : OUT LPAREN write_aux RPAREN SEMI

    '''
    global currLine
    currLine += 1
    for item in p[3]:
        if isinstance(item, str):
            quadruple.append(('OUT', item, None, None))
        else:
            quadruple.append(('OUT', item, None, None))
    p[0] = None

def p_write_aux(p):
    '''
    write_aux : write_aux2
              | write_aux2 COMMA write_aux

    '''
    if len(p) == 2:
        p[0] = [p[1]]
    elif len(p) == 4:
        p[0] = [p[1]] + p[3]



def p_write_aux2(p):
    '''
    write_aux2 : cvar
               | LETRERO

    '''
    if p[1] is not None:
        p[0] = p[1]

def p_forloop(p):
    '''
    forloop : FOR asignavar forJump texp forQuad DO bloque_use fill_for
    '''
    
def p_forJump(p):
    '''
    forJump :
    '''
    jumpStack.append(len(quadruple))

def p_forQuad(p):
    '''
    forQuad : 
    '''
    quadruple.append(('GOTOF',None,None,-1))
    jumpStack.append(len(quadruple)-1)

def p_fill_for(p):
    '''
    fill_for :
    '''
    gotoF = jumpStack.pop()
    goto = jumpStack.pop()
    jumpIndex = len(quadruple)
    quadruple.append(('GOTO', None, None, goto))
    quadruple[gotoF] = ('GOTOF', None, None, jumpIndex) 

def p_ifelse(p):
    '''
    ifelse : IF texp gotoQuadIf THEN bloque_use fill_if
           | IF texp gotoQuadIf THEN bloque_use fill_if ELSE bloque_use fill_else

    '''

def p_gotoQuad(p):
    '''
    gotoQuadIf : 
    '''
    quadruple.append(('GOTOF', None, None, None))
    jumpStack.append(len(quadruple) - 1)

def p_gotQuadelse(p):
    '''
    gotoQuadelse :
    '''
    quadruple.append(('GOTO', None, None, None))
    jumpStack.append(len(quadruple) - 1)


def p_fill_if(p):
    '''
    fill_if :
    '''
    fill_index = jumpStack.pop()
    jumpIndex = len(quadruple)
    quadruple[fill_index] = ('GOTOF', None, None, jumpIndex)

def p_fill_else(p):
    '''
    fill_else : 
    '''
    index = jumpStack.pop()
    jumpIndex = len(quadruple)
    quadruple[index] = ('GOTO', None, None, jumpIndex)


def p_while(p):
    '''
    while : WHILE gotoQuadWhile LPAREN texp RPAREN gotofQuadWhile DO bloque_use fill_while

    '''
def p_gotoQuadWhile(p):
    '''
    gotoQuadWhile :
    '''
    jumpStack.append(len(quadruple))

def p_gotofQuadWhile(p):
    '''
    gotofQuadWhile : 
    '''
    quadruple.append(('GOTOF', None, None, -1))
    jumpStack.append(len(quadruple)-1)

def p_fill_while(p):
    '''
    fill_while :
    '''
    gotof = jumpStack.pop()
    goto = jumpStack.pop()
    jumpIndex = len(quadruple)
    quadruple.append(('GOTO', None, None, goto))
    quadruple[gotof] = ('GOTOF', None, None, jumpIndex)


def p_cvar(p):
    '''
    cvar : ID
         | ID LBRACKET expr RBRACKET 

    '''
    if len(p) == 2:
        var_name = p[1]
        if tablaVars.exists_var(var_name):
            p[0] = tablaVars.get_var_mem(var_name)
        else:
            print(f"Error: Variable '{var_name}' has not been declared")
            p[0] = None
    else:
        element = str(p[1]) + str(p[2]) + str(p[3]) + str(p[4])
        if tablaVars.exists_var(element):
            p[0] = tablaVars.get_var_mem(element)
        else:
            var = p[3]
            index = tablaVars.get_value_by_memory(var)
            element = str(p[1]) + str(p[2]) + str(index) + str(p[4])
            p[0] = tablaVars.get_var_mem(element)


def p_var_array(p):
    '''
    vararr : type ID LBRACKET cvar RBRACKET SEMI
           | type ID LBRACKET lit RBRACKET SEMI
    '''
    global local
    if not local:
        scope = 'global'
    else:
        scope = 'local'
    arrType = p[1]
    arr_name = p[2]
    expr = p[4]
    var = tablaVars.get_name_by_mem(expr)
    if tablaVars.exists_var(var):
        size = tablaVars.get_value_by_memory(expr)
        allocate_array(arr_name, arrType, size, scope)
    else:
        allocate_array(arr_name, arrType, expr, scope)

def p_assign_array(p):
    '''
    asignaarr : ID LBRACKET expr RBRACKET EQUALS expr SEMI
    '''
    arrName = p[1]
    index = p[3]
    value = p[6]
    element = str(p[1]) + str(p[2]) + str(p[3]) + str(p[4])
    if not tablaVars.exists_array(arrName):
        print(f"Error: Array {arrName} does not exist")
        return
    if tablaVars.exists_var(element):
        index_val = tablaVars.get_var_mem(element)
        quadruple.append(('=', value, None, index_val))
    else:
        index = tablaVars.get_value_by_memory(index)
        element = str(p[1]) + str(p[2]) + str(index) + str(p[4])
        if tablaVars.exists_var(element):  
            index_val = tablaVars.get_var_mem(element)
            quadruple.append(('=', value, None, index_val))



def arr_value(arrName, index):
    base = tablaVars.get_var_mem(arrName)
    size = tablaVars.arr_size(arrName)
    memory = base + index * size
    return memory


def p_read(p):
    '''
    read : GET LPAREN ID RPAREN SEMI
         | GET LPAREN ID RPAREN SEMI read_aux

    '''
    variable = p[3]
    while True:
        if not tablaVars.exists_var(variable):
            print(f"Error: variable {variable} not found. You need to declare the variable first")
            break

        userInput = input(f"Enter value for {variable}: ")
        vartype = tablaVars.get_var_type(variable)

        if vartype == 'int':
            try:
                userInput = int(userInput)
            except ValueError:
                print(f"Invalid input. Please enter an integer for {variable}")
                continue
            tablaVars.set_var_value(variable, userInput)
            break
        elif vartype == 'float':
            try:
                userInput = float(userInput)
            except ValueError:
                print(f"Invalid input. Please enter a float for {variable}")
                continue
            tablaVars.set_var_value(variable, userInput)
            break
        else:
            print(f"Please enter a numerical value for {variable}")
            break
    p[0] = p[3]

def p_read_aux(p):
    '''
    read_aux :
             | read_aux2
    '''

def p_read_aux2(p):
    '''
    read_aux2 : COMMA GET LPAREN ID RPAREN SEMI
    '''
    variable = p[5]
    varMem = tablaVars.get_var_mem(variable)
    while True:
        userInput = input(f"Enter value for {variable}: ")
        vartype = tablaVars.get_var_type(variable)

        try:
            if vartype == 'int':
                userInput = int(userInput)
            elif vartype == 'float':
                userInput = float(userInput)
            else:
                raise ValueError(f"Invalid variable type: {vartype}")
        except ValueError as e:
            print(f"Invalid input type: {e}")
            continue

        quadruple.append(('=', userInput, None, varMem))
        break

    p[0] = p[3]


def p_body2(p):
    '''
    body2 : LBRACE bloque end RBRACE 
    '''
def p_end(p):
    '''
    end : 
    '''
    quadruple.append(('RETURN', None, None, None))


def p_body1(p):
    '''
    body1 : LBRACE bloque RETURN RBRACE

    '''
#def p_return(p):
 #   '''
  #  return : type
   #        | VOID
  #  '''
  #  p[0] = p[1]



def p_aexp(p):
    '''
    aexp : texp
         | texp AND texp
         | texp OR texp

    '''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 4:
        oper = p[2]
        left = p[1]
        right = p[3]
        temp = allocate_tempMem('bool')
        quadruple.append((oper, left, right, temp))
        tablaVars.add_temp(temp)
        p[0] = temp


def p_texp(p):
    '''
    texp : expr
         | expr LT expr
         | expr GT expr
         | expr LE expr
         | expr GE expr
         | expr EQ expr
         | expr NE expr
    '''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 4:
            oper = p[2]
            left = p[1]
            right = p[3]
            temp = allocate_tempMem('bool')
            quadruple.append((oper, left, right, temp))
            tablaVars.add_temp(temp)
            p[0] = temp



def p_expr(p):
    '''
    expr : term
         | expr PLUS term
         | expr MINUS term
    '''
    global currLine
    currLine += 1
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 4:
        oper = p[2]
        left = p[1]
        right = p[3]
        temp = allocate_tempMem('int')
        quadruple.append((oper, left, right, temp))
        tablaVars.add_temp(temp)
        p[0] = temp
    else:
        print("Error in p_expr rule")

def p_term(p):
    '''
    term : fact
         | term TIMES fact
         | term DIVIDE fact 
    '''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 4:
        oper = p[2]
        left = p[1]
        right = p[3]
        temp = allocate_tempMem('int')
        quadruple.append((oper, right, left, temp))
        tablaVars.add_temp(temp)
        p[0] = temp
    else:
        print("Error in p_term rule")
    
def p_fact(p):
    '''
    fact : LPAREN aexp RPAREN
         | lit 
         | cvar
         | cfunc
    '''
    if len(p) == 4:
        p[0] = p[2]
    else:
        if p[1] is not None:
            p[0] = p[1]

def p_cfunc(p):
    '''
    cfunc : ID LPAREN cvar RPAREN SEMI
          | ID LPAREN RPAREN SEMI
    '''
    if len(p) == 5:
        if directory.exists_func is not None:
            currIndex = len(quadruple)
            jump = funcJump.pop() + 1
            quadruple.append(('GOSUB', None, currIndex, jump))
        else:
            raise ValueError(f"Function: {p[1]}, does not exist")
    else:
        if directory.exists_func is not None:
            quadruple.append(('PARAM', funcParams.pop(), p[3], None))
            currIndex = len(quadruple)
            jump = funcJump.pop() + 1
            quadruple.append(('GOSUB', None, currIndex, jump))

    
def p_func(p):
    '''
    func : FUNCTION type ID eraQuad param body1 fillFunc
         | FUNCTION VOID ID eraQuad param body2 fillFunc
    '''
    global local
    local = True
    funcType = p[2]
    funcName = p[3]
    
    directory.add_function(funcName, funcType)

def p_funcVar(p):
    '''
    funcVar : type ID
    '''
    varName = p[2]
    varType = p[1]
    mem = allocate_memory('local', varType)
    tablaVars.add_var(varName, varType, 'local')
    tablaVars.set_var_mem(varName, mem)
    funcParams.append(mem)
    

def p_eraQuad(p):
    '''
    eraQuad :
    '''
    funcJump.append(len(quadruple))
    quadruple.append(('ERA', None, None, None))

def p_fillFunc(p):
    '''
    fillFunc :
    '''
    global local
    local = False
    endFunc = len(quadruple)
    currIndex = funcJump.pop()
    quadruple[currIndex] = ('ERA', None, None, endFunc)
    funcJump.append(currIndex)

def p_param(p):
    '''
    param : LPAREN RPAREN
          | LPAREN param_aux RPAREN
    '''

def p_param_aux(p):
    '''
    param_aux : funcVar
              | funcVar COMMA param_aux
    '''


def p_fun_esp(p):
    '''
    fun_esp : draw_line
    '''

def p_endfunc(p):
    '''
    endfunc :
    '''
    global local
    local = False
    quadruple.append(('END', None, None, None))

def p_draw_line(p):
    '''
    draw_line : DRAW CTEI CTEI CTEI CTEI
    '''
    x1 = p[2]
    y1 = p[3]
    x2 = p[4]
    y2 = p[5]
    gra.draw_line(x1, y1, x2, y2)

def p_empty(p):
    '''
    empty :
    '''
    pass

def p_error(p):
    print(f"Syntax error at line {p.lineno}, position {p.lexpos}, token {p.type}")

parser = yacc.yacc()

with open('test3.txt', 'r') as file:
    code = file.read()

lexer.input(code)

#for token in lexer:
 #   print(token)

result = parser.parse(code, lexer=lexer)


def execute_quadruples(quadruples):
    return_stack = []
    index = 0
    result_stack = []
    Boolstack = []

    while index < len(quadruples):
        quadruple = quadruples[index]
        if quadruple is None: 
            continue
        op, arg1, arg2, result = quadruple

        if op == 'GOTOF':
            if result is not None:
                if not Boolstack:
                    print("Boolstack is empty")
                condition = Boolstack.pop()
                if not condition:
                    jumpIndex = result
                    index = jumpIndex + 1
                    continue
                else:
                    index += 1
                    continue

        elif op == 'GOTO':
            index = result
            continue

        elif op == '=':
            arg1 = get_value(arg1)
            tablaVars.set_var_valuefromMem(result, arg1)
            result_stack.append(result)

        elif op == 'OUT' or op == 'out':
            arg1 = get_value(arg1)
            if arg1 is not None:
                print(arg1)

        elif op in ('<', '>', '<=', '>=', '==', '!=', 'AND', 'OR'):
            arg1 = get_value(arg1)
            arg2 = get_value(arg2)


            if op == '<':
                Boolstack.append(arg1 < arg2)
            elif op == '>':
                Boolstack.append(arg1 > arg2)
            elif op == '<=':
                Boolstack.append(arg1 <= arg2)
            elif op == '>=':
                Boolstack.append(arg1 >= arg2)
            elif op == '==':
                Boolstack.append(arg1 == arg2)
            elif op == '!=':
                Boolstack.append(arg1 != arg2)
            elif op == 'AND':
                Boolstack.append(arg1 and arg2)
            elif op == 'OR':
                Boolstack.append(arg1 or arg2)
                    
        elif op == '+' or op == '-' or op == '*' or op == '/':
            arg1_val = get_value(arg1)
            arg2_val = get_value(arg2)

            if arg1_val is None or arg2_val is None:
                print("Error: Operand(s) not found in symbol table")
                return None

            if op == '+':
                    result_value = arg1_val + arg2_val
            elif op == '-':
                    result_value = arg1_val - arg2_val
            elif op == '*':
                    result_value = arg1_val * arg2_val
            elif op == '/':
                    result_value = arg2_val / arg1_val

            tablaVars.set_temp_value(result, result_value)
            result_stack.append(result_value)

        elif op == 'PARAM':
            arg2 = get_value(arg2)
            tablaVars.set_var_valuefromMem(arg1, arg2)

        elif op == 'ERA':
            return_stack.append(index + 1)
            index = result
            continue
        elif op == 'GOSUB':
            return_stack.append(index + 1)
            index = result
            continue

        elif op == 'RETURN':
            return_address = return_stack.pop()
            index = return_address
            continue

        
        elif op == 'ENDPROG':
            exit

        index += 1
        

    return None

def get_value(operand):
    for variable in tablaVars.variables.values():
        if variable.mem == operand:
            return variable.value
    for temp in tablaVars.temps.values():
        if temp.mem == operand:
            return temp.value
    else:
        return operand
    
execute_quadruples(quadruple)










