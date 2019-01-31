use "test.sml";

signature prettyPrinting = sig
  type t
  val conv_mod : ast.Module -> t
end

structure convert:prettyPrinting = struct 
	type t = string list;

	fun get_name (ast.Name x) = x;

	fun get_type x = case x of
			  (ast.IntegerType y)  => "i" ^ IntInf.toString(y)
			 |(ast.FloatingType ast.HalfFP)      => "f16"
	   		 |(ast.FloatingType ast.FloatFP)     => "f32"
	   		 |(ast.FloatingType ast.DoubleFP)    => "f64"
	   		 |(ast.FloatingType ast.FP128FP)     => "f128"
	   		 |(ast.FloatingType ast.X86_FP80FP)  => "f80"
	   		 |(ast.FloatingType ast.PPC_FP128FP) => "f128";

	fun get_value (SOME (ast.Int x)) = x;

	fun get_returnType x = case x of
	 					ast.VoidType                 => "void"
	                   |ast.IntegerType y            => "i" ^ IntInf.toString(y);
	                   
	fun get_visibility x = case x of
	                    ast.Default => "@"
	                    |_           => "%";

	fun get_parameters (ast.Parameter (x,y,_)) = get_type(x) ^ " " ^ get_name(y);

	fun get_para_helper []      = ""
	   |get_para_helper (x::xs) = case xs of
								 [] => get_parameters(x)
								|_  => get_parameters(x) ^ " , " ^ get_para_helper(xs);

	fun get_para (x,y) = case y of 
						false => ""
					   |true  =>  get_para_helper(x);

	fun get_bb_name (ast.Name x) = [x ^ ":"];

	fun get_terminator (ast.Do x) = [];

	fun get_values (ast.LocalReference (x,ast.Name y)) = y;  

	fun get_ret_type (ast.LocalReference (x,_)) = case x of
										ast.IntegerType 32 => "i32"
									   |_                  => "";

	fun get_decode_inst (ast.Add x) = "add " ^ get_ret_type(#operand0(x)) ^ " " ^ "%" ^ get_values(#operand0(x)) ^ " " ^ "%" ^ get_values(#operand1(x));

	fun get_inst (ast.Named (x,y)) = [ x ^ " = " ^ get_decode_inst(y)];

	fun get_instruction_set nil     = []
	   |get_instruction_set (x::xs) = get_inst(x) @ get_instruction_set(xs); 


	fun get_block (ast.BasicBlock (x,y,z)) = get_bb_name(x) @ get_instruction_set(y) @ get_terminator(z) ;

	fun get_bb []      = []
	   |get_bb (x::xs) = (get_block x) @ (get_bb xs);


	fun conv_global (ast.GlobalVariable x) = ["@" ^ get_name(#name(x)) ^ " = global " ^ get_type(#types(x)) ^ " " ^ IntInf.toString(get_value(#initlizer(x))) ^ " "]
	   |conv_global (ast.Function x)       = ["define " ^ get_returnType(#returnType(x)) ^ " " ^ get_visibility(#visibility(x)) ^ get_name(#name(x)) ^ "(" ^ get_para(#parameters(x)) ^ ") {"] @ get_bb(#basicblock(x)) @ ["}"] 
	   |conv_global _                      = [""];

	fun conv_def [] = []
	   |conv_def (x::xs) = case x of
						ast.GlobalDefinition y => ((conv_global y) @ (conv_def xs));

	fun conv_mod (ast.Module x) = conv_def (#moduleDefination x);
end;


val d = convert.conv_mod defaultModule;

fun quit(outFile: string, list: string list) =
  let
    val outStream = TextIO.openOut outFile
    fun out(xs : string list) =  
          case xs of
              [] => (TextIO.closeOut outStream)
            | x::xs' => (TextIO.output(outStream, x ^ "\r\n"); out(xs'))
  in
    out(list)
  end;

val e = quit(outputFile,d);