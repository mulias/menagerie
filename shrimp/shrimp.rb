class Shrimp

  attr_reader :exps, :results

  def initialize (input = "")
    @exps = parse_all(tokenize(input)).map { |ast| allsteps(ast) }
	@results = @exps.map { |e| print(e) }
  end
  
  def extend (input)
    exps = parse_all(tokenize(input)).map { |ast| allsteps(ast) }
	results = exps.map { |e| print(e) }
	@exps.concat(exps)
	@results.concat(results)
	results
  end
 
  #
  # Define Types
  #
  Typ_A = Struct.new(:typ1, :typ2)
  Typ_I = Struct.new(:sym)
  Typ_B = Struct.new(:sym)
  Typ_U = Struct.new(:sym)
  
  def is_typ? (t)
    [Typ_A, Typ_I, Typ_B, Typ_U].any? { |typ| t.is_a? typ }
  end
  
  #
  # Define Expressions
  #
  Var = Struct.new(:sym)
  App = Struct.new(:exp1, :exp2)
  Lam = Struct.new(:var, :typ, :bod)
  Int = Struct.new(:int)
  Boo = Struct.new(:bool)
  Let = Struct.new(:env)

  def is_exp? (e)
    [Var, App, Lam, Int, Boo].any? { |exp| e.is_a? exp }
  end
    
  def is_val? (v)
    [Lam, Int, Boo].any? { |val| v.is_a? val }
  end
  
  #
  # Build Types
  #
  def typ_a (t1, t2)
    if (is_typ? t1) && (is_typ? t2) then Typ_A.new(t1, t2)
	else raise end
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
   
  #
  # Build Variable
  #
  def var (sym)
    if (sym.is_a? Symbol) then Var.new(sym)
	else raise end
  end

  $var_count = 0

  def fresh_var
	$var_count += 1
	var(("_" + $var_count.to_s).to_sym)
  end

  #
  # Build Application
  #
  def app (e1, e2)
    if (e1.is_a? Lam) && (is_exp? e2) then App.new(e1, e2)
	else raise end
  end

  #
  # Values
  #
  def lam (var, typ, bod)
    if (var.is_a? Var) && (is_typ? typ) && (is_exp? bod) then Lam.new(var, typ, bod)
	else raise end
  end

  def int (i)
    if (i.is_a? Integer) then Int.new(i)
	else raise end
  end

  def boo (b)
    if (b.is_a? TrueClass) || (b.is_a? FalseClass) then Boo.new(b)
	else raise end
  end


  #
  # Simplification
  #
  def step (exp)
	case exp
	  when Var then raise
	  when App then
		e1, e2 = exp.exp1, exp.exp2
		if    !(is_val? e1) then app(step(e1), e2)
		elsif !(is_val? e2) then app(e1, step(e2))
		else subst(e1.var, e2, e1.bod) end
	  when Lam then exp
	  when Int then exp
	  when Boo then exp
	  else raise
	end
  end

  def subst (v, newv, exp)
	case exp
	  when Var then
		if exp == v then newv
		else exp end
	  when App then
		e1, e2 = exp.exp1, exp.exp2
		app(subst(v, newv, e1), subst(v, newv, e2))
	  when Lam then
		lv, lt, lb = exp.var, exp.typ, exp.bod
		if lv == v then exp
		else lam(lv, lt, subst(v, newv, lb)) end
	  when Int then exp
	  when Boo then exp
	  else raise
	end
  end

  def allsteps (exp)
	while !(is_val? exp) do exp = step(exp) end
	exp
  end

  #
  # Parsing
  #
  def tokenize (str)
	acc = []
	str.chars.each do |c|
	  case c
		when '('  then acc << :LP 
		when ')'  then acc << :RP
		when '['  then acc << :LS 
		when ']'  then acc << :RS
		when ':'  then acc << :COL
		when '.'  then acc << :DOT
		when '='  then acc << :EQU
		when '&'  then acc << :AMP
		when ' '  then acc << nil
		when "\n" then acc << nil
		when "\r" then acc << nil
		when "\t" then acc << nil
		else
		  if (acc.last.kind_of? String) then acc.last << c
		  else acc << c end
	  end
	end
	acc.compact
  end

  def parse_all (tokens)
    trees = []
    while !tokens.empty? do trees << parse(tokens) end
	trees
  end
	
  def parse (tokens)
    t = tokens.shift
    case t
  	when :LP  then parse_app(tokens)
  	when :LS  then parse_lam(tokens)
  	when :RP  then raise
  	when :RS  then raise
  	when :COL then raise
  	when :DOT then raise
  	when :EQU then raise
	when :AMP then parse_let
  	else
  	  if    (i = parse_int(t)) then i
  	  elsif (b = parse_boo(t)) then b
  	  elsif (v = parse_var(t)) then v
  	  else raise end
    end
  end

  def parse_app (tokens)
	a = app(parse(tokens), parse(tokens))
	if tokens.shift != :RP  then raise end
	a
  end

  def parse_lam (tokens)
	v = parse_var(tokens.shift)
	if tokens.shift != :COL then raise end
	t = parse_typ(tokens)
	if tokens.shift != :DOT then raise end
	b = parse(tokens)
	if tokens.shift != :RS then raise end
	lam(v, t, b)
  end

  def parse_int (t)
	if t.kind_of? String then
	  i = t.to_i
	  if i.to_s == t then int(i)
	  else nil end
	else raise end
  end

  def parse_boo (t)
	if t.kind_of? String then
	  if t == "true" then boo(true)
	  elsif t == "false" then boo(false)
	  else nil end
	else raise end
  end

  def parse_var (t)
	if t.kind_of? String then
	  if t[0] != "_" then var(t.to_sym)
	  else nil end
	else raise end
  end

  def parse_typ (tokens)
	t = tokens.shift
	if t.kind_of? String then
	  if    t == "i" then typ_i
	  elsif t == "b" then typ_b
	  elsif t == "u" then typ_u
	  elsif t.length > 1 && t[0] == '>' then
		t1 = parse_typ(tokens.unshift(t[1..-1]))
		if tokens.shift != :COM then raise end
		t2 = parse_typ(tokens)
		typ_a(t1, t2)
	  else raise end
	else raise end
  end

  #
  # Shrimp to Ruby
  #
  def shrimp_to_ruby (v)
    case v
	  when Int then v.int
	  when Boo then v.bool
	  when Lam then eval "lambda { |#{l.var.sym.to_s}| shrimp #{print(l)} }"
	  else raise
	end
  end

  #
  # Ruby to Shrimp
  #
  def ruby_to_shrimp (r)
    if    r.is_a? Integer    then int(r)
	elsif r.is_a? TrueClass  then boo(true)
	elsif r.is_a? FalseClass then boo(false)
	elsif r.is_a? Proc       then lam(fresh_var, typ_u, "lol")
	else raise end
  end

  #
  # Printing
  #
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
	  when Boo   then exp.bool.to_s
	  else raise
	end
  end

end


#
# Running Shrimp code
#
def run_shrimp (input, debug: false)
  if debug
    s = Shrimp.new()
    puts "input #{input}"
    tokens = s.tokenize(input)
    puts "tokens #{tokens}"
    parsed = s.parse_all(tokens)
    puts "parsed #{parsed}"
    stepped = parsed.map { |e| s.allsteps(e) }
    puts "stepped #{stepped}"
    stepped.map { |v| s.print(v) }
  else
    Shrimp.new(input).results
  end
end









