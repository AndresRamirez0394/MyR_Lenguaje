
class Variable:
    def __init__(self, name, vartype, scope, size = None):
        self.name = name
        self.vartype = vartype
        self.scope = scope
        self.mem = None
        self.value = None
        self.size = size
    
    def set_value(self, new_value):       
        self.value = new_value
    def set_mem(self, new_mem):
        self.mem = new_mem
    def set_size(self, new_size):
        self.size = new_size

class Array:
    def __init__(self, name, vartype, scope, size):
        self.name = name
        self.vartype = vartype
        self.scope = scope
        self.size = size
        self.mem = None

    def set_mem(self, new_mem):
        self.mem = new_mem

class Temps:
    def __init__(self, mem):
        self.mem = mem
        self.value = None
    
    def set_value(self, new_value):       
        self.value = new_value
    def set_mem(self, new_mem):
        self.mem = new_mem


#class to define Variables
class tabla_Vars:
    def __init__(self):
        self.variables = {}
        self.temps = {}
        self.arrays = {}

    def add_array(self, name, vartype, scope, size):
        if name in self.arrays:
            raise ValueError(f"Array '{name}' has already been declared.")
        array = Array(name, vartype, scope, size)
        self.arrays[name] = array

    def get_array_size(self, name):
        array = self.arrays.get(name, None)
        if array:
            return array.size
        else:
            raise ValueError(f"Array '{name}' does not exist")
        
    def exists_array(self, name):
        return name in self.arrays

    def add_temp(self, mem):
        if mem in self.temps:
            mem += 1
        temp = Temps(mem)
        self.temps[mem] = temp
    
    def get_temp(self, mem):
        return self.temps.get(mem, None)

    def set_temp_value(self, mem, new_value):
        temp = self.get_temp(mem)
        if temp:
           temp.set_value(new_value)
        else:
            print("Temp Error")

    def get_tempVal_memory(self, mem):
        temp = self.get_temp(mem)
        if temp:
            return temp.value
        else:
            raise ValueError(f"Error in temps")
    

    #add global variable to table
    def add_var(self, name, vartype, scope):
        if name in self.variables:
            raise ValueError(f"Variable '{name}' has already been declared.")
        variable = Variable(name, vartype, scope)
        self.variables[name] = variable

    def get_var(self, name):
        return self.variables.get(name, None)

    #check if variable exists
    def exists_var(self, name):
        return name in self.variables
    
    def set_var_value(self, name, new_value):
        variable = self.get_var(name)
        if variable:
            variable.set_value(new_value)
        else:
            raise ValueError(f"Variable '{name}' does not exist")
    
    def set_var_valuefromMem(self, mem, new_value):
        variable = self.get_var_by_mem(mem)
        if variable:
            variable.set_value(new_value)
    
    def set_var_mem(self, name, new_mem):
        variable = self.get_var(name)
        if variable:
            variable.set_mem(new_mem)
        else:
            raise ValueError(f"Variable '{name}' does not exist")
        
    def get_var_mem(self, name):
        variable = self.get_var(name)
        if variable:
            return variable.mem
        else:
            raise ValueError(f"Variable memory '{name}' does not exist")
    
    def get_var_type(self, name):
        variable = self.get_var(name)
        if variable:
            return variable.vartype
        else:
            raise ValueError(f"Variable '{name}' does not exist")

    def get_var_value(self, name):
        variable = self.get_var(name)
        if variable:
            return variable.value
        else:
            raise ValueError(f"Variable '{name}' does not exist")
    
    def get_value_by_memory(self, memory):
        for variable in self.variables.values():
            if variable.mem == memory:
                return variable.value
        raise ValueError(f"No variable found with memory address {memory}")
        
    def get_type_by_memory(self, memory):
        for variable in self.variables.values():
            if variable.mem == memory:
                return variable.vartype
            
    def get_var_by_mem(self, mem):
        for variable in self.variables.values():
            if variable.mem == mem:
                return variable
        return None        
    
    def get_name_by_mem(self, mem):
        for variable in self.variables.values():
            if variable.mem == mem:
                return variable.name
        return None
    
    def replace_var(self, old_var_mem, new_var_mem):
        old_var = None
        for var in self.variables.values():
            if var.mem == old_var_mem:
                old_var = var
                break

        if old_var:
            del self.variables[old_var.name]
            new_var = Variable(old_var.name, old_var.vartype, old_var.scope)
            new_var.set_mem(new_var_mem)
            self.variables[old_var.name] = new_var
        else:
            print(f"Error: Variable with memory {old_var_mem} not found")

    #Return the size of a declared array
    def arr_size(self, name):
        variable = self.get_var(name)
        if variable:
            return variable.size
        else:
            raise ValueError(f"Array '{name}' does not exist")

        

#class to define functions
class Function:
    
    #constructor for class functions
    def __init__(self, name, funcType):
        self.name = name
        self.funcType = funcType
        self.mem = None
        self.vars = tabla_Vars()

    def set_mem(self, new_mem):
        self.mem = new_mem

        
class func_Dir:
    def __init__(self):
        self.functions = {
            'local': {'type': 'void', 'vars': {}, 'params': []}
        }

    def add_function(self, name, funcType):
        if name not in self.functions:
            self.functions[name] = {'type': funcType, 'vars': {}, 'params': [], 'numQuads': None}
            if funcType != 'void':
                self.functions['global']['vars'][name] = {
                    'type': funcType,
                }
            return True
        else:
            print(f"Error: Function '{name}' already exists")
            return False
        
    def get_func(self, name):
        return self.functions.get(name)
    
    def exists_func(self, name):
        return name in self.functions

    def set_func_mem(self, name, new_mem):
        fun = self.get_func(name)
        if fun:
            fun.set_mem(new_mem)
        else:
            print(f"Error: function does not exist")


    
        