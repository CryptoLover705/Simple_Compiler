=begin
    The goal here is to build a very simple compiler, for a very simple language. We will have very simple constructs for now like def f(x,y) add(42, add(x,y)) end, which says in English, define a function f that takes 2 arguments (x,y) and returns the return value of add(42, add(x, y)). This could easily be much more complex, but I wanted to keep things simple for high level understanding. Feel free to expand on this in your own languages 😉
    
    This code will consist of 3 main components: a lexer (or tokenizer), a parser, and a code generator.
    
    Note that this code does NOT use the most efficient practices for compiler building, but rather the simplest approach for understanding purposes. 
    
    The simple version of this compiles the code down to python. feel free to test it! 😎
    
    While I do work in the system field, compiler design is not my area of expertise, and I referred to a number of written sources to put this together.
    
    @uthor: CryptoLover705
    Bugs: None known
    Version:
    20180505 - Lexer class added
    20180515 - Lexer complete, begin Parser
    20180520 - Parser complete, Start generation 
    20180524 - CodeGen complete, code release
    
=end

class Lexer
    T_TYPES = [
               [:define, /\bfun\b/],
               [:func_end, /\bend\b/],
               [:identifier, /\b[a-zA-Z]+\b/],
               [:integer, /\b[0-9]+\b/],
               [:oparan, /\(/],
               [:cparan, /\)/],
               [:comma, /\,/]
                ]
    def initialize(code)
       @code = code
       print "***TEST CODE***\n#{@code}\n\n"
   end
   
   #This method is responsible for breaking 
   #the code into the smallest meaningful 
   #units possible
   def tokenize
       tokens = []
       until @code.empty?
           tokens << getToken
           @code = @code.strip
       end
       tokens 
   end
   
   def getToken
       T_TYPES.each do |type, re|
           #make sure we get the true 
           #beginning of the string
           re = /\A(#{re})/
           #check for regex match
           if @code =~ re
               val = $1
               @code = @code[val.length..-1]
               return Token.new(type, val)
           end
       end
       raise RuntimeError("Could not retrieve token for #{@code.inspect}")
   end
end

Token = Struct.new(:type, :value)

# Typically these tokens would be read from a
# source file, but in an effort to keep 
# things simple, we won't worry about that 
# for now. 
tokens = Lexer.new("fun f(x,y) add(42, add(x,y)) end").tokenize
print "***TESTING LEXER***\n"
print tokens.map(&:inspect).join("\n")

#Tokenizer complete. Step 2 - Parse the tokens
class Parser 
    #A parser translates the tokens into  
    #tree representing structure of the code
    def initialize(tokens)
        @tokens = tokens 
    end
    
    def parse
        defParse
    end
    
    def defParse 
        consume(:define)
        name = consume(:identifier).value 
        args = getArgs #could be 0 or more
        body = getBody #could be a complex expression
        DefNode.new(name, args, body)
    end
    
    def getArgs
        args = []
        consume(:oparan)
        #do we have argument(s)?
        if(peek(:identifier))
            args << consume(:identifier).value
            while(peek(:comma))
                consume(:comma)
                args << consume(:identifier).value
            end
        end
        consume(:cparan)
        args
    end
    
    def peek(expectedType, offset=0)
        @tokens.fetch(offset).type == expectedType
    end
    
    def getBody
        if peek(:integer)
            parseInt
        elsif peek(:identifier) && peek(:oparan, 1)
            parseCall
        else 
            parseVar
        end
    end
    
    def parseVar 
        VarNode.new(consume(:identifier).value)
    end
    
    def parseCall 
        name = consume(:identifier)
        exp = parseArgExp
        CallNode.new(name, exp)
    end
    
    def parseArgExp
        argExp  = []
        consume(:oparan)
        #do we have argument(s) or expression(s)?
        if(!peek(:cparan))
            argExp << parseExp
            while(peek(:comma))
                consume(:comma)
                argExp << parseExp
            end
        end
        consume(:cparan)
        argExp
    end
    
    def parseExp
    #Expressions can be one of the following:
    #1) Integers
    #2) function calls
    #3) variables
        if peek(:integer)
            parseInt 
        elsif peek(:identifier) && peek(:oparan, 1)
            parseCall 
        else
            parseVar 
        end
    end
    
    def parseInt 
        IntNode.new(consume(:integer).value.to_i)
    end
    
    def consume(expectedType)
        token = @tokens.shift
        if token.type == expectedType
            token 
        else
            raise RuntimeError.new("Expected #{expectedType} but received #{token.type}")
        end
    end
end

DefNode = Struct.new(:name, :args, :body)
IntNode = Struct.new(:value)
CallNode = Struct.new(:name, :argExp)
VarNode = Struct.new(:value)

tokenTree = Parser.new(tokens).parse
puts "\n\n***TESTING PARSER***"
print tokenTree.map(&:inspect).join("\n")

#Parsing complete. Last is code generation
puts "\n\n***TESTING CODE GENERATOR***\n"
class CodeGen 
    def generate(node)
        case node
        when DefNode
            "def %s(%s):\n\treturn %s" % 
            [node.name,
            node.args.join(","),
            generate(node.body)
            ]
        when CallNode 
        "%s(%s)" % [node.name.value,
                    node.argExp.map{ 
                    |exp| generate(exp)
                    }.join(",")]
        when IntNode 
        node.value 
        when VarNode 
        node.value 
        else raise RuntimeError.new("Unexpected node type: #{node.class}")
        end
    end
end

generated = CodeGen.new.generate(tokenTree)
aux = "\n\ndef add(x,y):\n\treturn x+y
\nprint(f(-1, -10))"
print generated + aux
puts "\n***TRY THIS GENERATED CODE IN PYTHON***"
