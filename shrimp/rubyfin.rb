# typ ::= typ->typ | i | b
# exp ::= var | app | lam | int | arb
# val ::= lam | int | boo

Typ_A = Struct.new(:typ1, :typ2)
Typ_I = Struct.new(:typ)
Typ_B = Struct.new(:typ)
Typ_U = Struct.new(:typ)
Var = Struct.new(:sym)
Lam = Struct.new(:var, :typ, :bod)
App = Struct.new(:exp1, :exp2)
Int = Struct.new(:int)
Boo = Struct.new(:boo)
Arb = Struct.new(:inputs, :block, :typ) 

def typ_a (t1, t2)
  Typ_A.new(t1, t2)
end

def typ_i
  Typ_I.new(:i)
end

def typ_b
  Typ_B.new(:b)
end

def typ_u
  Typ_U.new(:u)
end

def var (sym)
  Var.new(sym)
end

def lam (var, type, body)
  Lam.new(var, type, body)
end

def app (e1, e2)
  App.new(e1, e2)
end

def int (i)
  Int.new(i)
end

def boo (b)
  Boo.new(b)
end

def arb (inputs, block, typ)
  Arb.new(inputs, block, typ)
end

def is_val? (exp)
  c = exp.class
  (c == Lam) || (c == Int) || (c == Boo)
end

def get_val (exp)
  case exp
    when Int then exp.int
	when Boo then exp.boo
  end
end

$var_count = 0

def fresh_var
  $var_count += 1
  var(("_" + $var_count.to_s).to_sym)
end

def step (exp)
  case exp
    when Lam then exp
    when Int then exp
    when Boo then exp
    when App then
      e1, e2 = exp.exp1, exp.exp2
      if    !(is_val? e1) then app(step(e1), e2)
      elsif !(is_val? e2) then app(e1, step(e2))
      else subst(e1.var, e2, e1.bod) end
    when Arb then execute_ruby(exp)
  end
end

def subst (v, newv, exp)
  case exp
    when Var then
      if exp == v then newv
      else exp end
    when Lam then
      lv, lt, lb = exp.var, exp.typ, exp.bod
      if lv == v then exp
      else lam(lv, lt, subst(v, newv, lb)) end
    when App then
      e1, e2 = exp.exp1, exp.exp2
      app(subst(v, newv, e1), subst(v, newv, e2))
    when Int then exp
    when Boo then exp
    when Arb then 
	  # TODO: if v is in inputs, then string sup newv into the block. remove v from inputs
	  newins = exp.inputs.map { |input| if input == v then newv else input end }
	  arb(newins, exp.block, exp.typ)
  end
end

def execute_ruby (exp)
  # TODO: if inputs is empty, run eval
  if exp.inputs.all? { |input| is_val? input }
    inputs = exp.inputs.map { |input| get_val(input) }
    res = exp.lambda.call(*inputs)
    case exp.typ
	  when Typ_I then int(res)
	  when Typ_B then boo(res)
    end
  else
    lam(fresh_var, typ_u, exp)
  end
end

def allsteps (exp)
  while !(is_val? exp) do exp = step(exp) end
  exp
end

def tokenize (str)
  lit_block = false
  acc = []
  str.chars.each do |c|
    if !lit_block || c == '}'
      case c
	    when '(' then acc << :LP 
	    when ')' then acc << :RP
	    when '[' then acc << :LS 
	    when ']' then acc << :RS
		when '{' then acc << :LB
                      lit_block = true
	    when '}' then acc << :RB
	                  lit_block = false
	    when ':' then acc << :COL
	    when '.' then acc << :DOT
        when ',' then acc << :COM
	    when ' ' then acc << nil
	    else
	      if (acc.last.kind_of? String) then acc.last << c
          else acc << c end
	  end
	else
	  if (acc.last.kind_of? String) then acc.last << c
      else acc << c end
	end
  end
  acc.compact
end

def parse (tokens)
  t = tokens.shift
  case t
    when :LP  then parse_app(tokens)
	when :LB  then parse_arb(tokens)
	when :LS  then parse_lam(tokens)
    when :RP  then "exception0"
    when :RB  then "exception1"
    when :RS  then "exception2"
    when :COL then "exception3"
    when :DOT then "exception4"
	when :COM then "exception5"
    else
	  if    (i = parse_int(t)) then i
	  elsif (b = parse_boo(t)) then b
	  elsif (v = parse_var(t)) then v
	  else "exception6" end
  end
end

def parse_app (tokens)
  a = app(parse(tokens), parse(tokens))
  if tokens.shift != :RP  then "exception22" end
  a
end

def parse_arb (tokens)
  ins, lam = parse_block(tokens.shift)
  if tokens.shift != :RB  then "exception7" end
  if tokens.shift != :COL then "exception8" end
  typ = parse_typ(tokens)
  arb(ins, lam, typ)
end

def parse_lam (tokens)
  v = parse_var(tokens.shift)
  if tokens.shift != :COL then "exception9" end
  t = parse_typ(tokens)
  if tokens.shift != :DOT then "exception10" end
  b = parse(tokens)
  if tokens.shift != :RP then "exception11" end
  lam(v, t, b)
end

def parse_int (t)
  if t.kind_of? String then
    i = t.to_i
    if i.to_s == t then int(i)
    else nil end
  else nil end
end

def parse_boo (t)
  if t.kind_of? String then
    if t == "true" then boo(true)
    elsif t == "false" then boo(false)
    else nil end
  else nil end
end

def parse_var (t)
  if t.kind_of? String then
    if t[0] != "_" then var(t.to_sym)
    else nil end
  else nil end
end

def parse_typ (tokens)
  t = tokens.shift
  if t.kind_of? String then
    if    t == "i" then typ_i
    elsif t == "b" then typ_b
    elsif t == "u" then typ_u
    elsif t.length > 1 && t[0] == '>' then
      t1 = parse_typ(tokens.unshift(t[1..-1]))
	  if tokens.shift != :COM then "exception17" end
	  t2 = parse_typ(tokens)
	  typ_a(t1, t2)
    else "exception19" end
  else "exception20" end
end

# {$x == $y}:b
# l = lambda { |x, y| eval 'x.to_s == y.to_s' }
# l.call(2,2) => true
def parse_block (str)
  # TODO: don't build a lambda, just do string sub
  if str.kind_of? String then
    var_strs = str.split.find_all { |v| /\$[a-z]/.match(v) }
	stripped_var_strs = var_strs.map { |v| v[1..-1] }
	vars = stripped_var_strs.map { |v| var(v.to_sym) }
	var_strs.each do |var|
	  str.sub!(var, "#{var[1..-1]}.to_s")
	end
	lam = eval "lambda { |#{stripped_var_strs.join(', ')}| eval '#{str}' }"
	return vars, lam
  else "exception21" end
end

def repl
  while (str = gets.chomp)
    out = allsteps(parse(tokenize(str)))
    puts print(out)
  end
end

def print (exp)
  case exp
    when Typ_A then [">",print(exp.typ1),",",print(exp.typ2)].join
    when Typ_I then "i"
	when Typ_B then "b"
	when Typ_U then "u"
	when Var   then exp.sym.to_s
	when Lam   then
	  ["[",print(exp.var),":",print(exp.typ),".",print(exp.bod),"]"].join
    when App   then ["(",print(exp.exp1)," ",print(exp.exp2),")"].join
	when Int   then exp.int.to_s
	when Boo   then exp.boo.to_s
	when Arb   then ["{...}",":",print(exp.typ)].join
  end
end





"""
([x:i.{|x|x+1}] 4) => 5
"""













