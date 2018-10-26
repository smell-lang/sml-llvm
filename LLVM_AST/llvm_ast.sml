(*********************************************************************************)

(*                        Project : LLVM bindings for sml                        *)
(*                        Program : llvm_ast.sml                                 *)
(*                        Author  : Rahul Dhawan                                 *)
(*                        Purpose : To capture the AST for LLVM                  *)

(*********************************************************************************)


(* Name can be used for the variable and function *)

datatype Name = Name of string
              | UnName of int 
              
(* constants *)

datatype Constant = Int of int
                  | Float of real
                  | NONE

(* Parameter attributes are used to communicate additional information about the result or parameters of a function *)

datatype ParameterAttribute = ZeroExt (*     This indicates to the code generator that the parameter or return value should be zero-extended to the extent required by the target’s ABI( application binary interface ) by the caller (for a parameter) or the callee (for a return value). *)
                            | SignExt (* This indicates to the code generator that the parameter or return value should be sign-extended to the extent required by the target’s ABI (which is usually 32-bits) by the caller (for a parameter) or the callee (for a return value). *)
                            | InReg (*  *)
                            | SRet (* *)
                            | Alignment of int (* *)
                            | NoAlias (* *)
                            | ByVal (* *)
                            | NoCapture (* *)
                            | Nest (* *)
                            | ReadNone (* *)
                            | ReadOnly (* *)
                            | WriteOnly (* *)
                            | InAlloca (* *)
                            | NonNull (* This indicates that the parameter or return pointer is not null. *)
                            | Dereferenceable of int (* *)
                            | DereferenceableOrNull of int (* *)
                            | Returned (* *)
                            | SwiftSelf (* *)
                            | SwiftError (* *)
                            | StringAttribute of string * string (* *)


(* used for capturing the floating point *)

datatype FloatingPointType = HalfFP      (* 16-bit floating point value *)
                           | FloatFP     (* 32-bit floating point value *)
                           | DoubleFP    (* 64-bit floating point value *)
                           | FP128FP     (* 128-bit floating point value (112-bit mantissa) *)
                           | X86_FP80FP  (* 80-bit floating point value (X87) *)
                           | PPC_FP128FP (* 128-bit floating point value (two 64-bits) *)

(* types used in llvm *)

datatype Type = VoidType (* does not represent any value and has no size *)
              | IntegerType of int
              | FloatingType of FloatingPointType
              | PointerType of Type * int (* used for capturing the pointer as type to which it pointed and the addresss *)
              | FunctionType of Type * Type list (* used for signature of the function return type and lis of parameter *)
              | VectorType of int * Type (* used for capturing array of vector with size and type *)
              | StructureType of bool * Type list (* sed to represent a collection of data members together in memory and the bool representing the padding between the elements *)
              | ArrayType of int * Type (* used for capturing array of  element with size and type *)
              | NamedTypeReference of Name
              | MetadataType (* used for parameter type for some instructions *)
              | LabelType   (* used for the type of block name *)
              | TokenType
            
(* void *)
val void = VoidType

(* 1 bit unsigned integer *)
val i1 = IntegerType 1

val i8 = IntegerType 8

val i16 = IntegerType 16

val i32 = IntegerType 32

val i128 = IntegerType 128


(* The signature of the module *)
(* module is basically translation unit of the input programm *)
(* module basically contain global var, local var and fun *)

signature Module = 
sig
	val modulName : string
	val moduleSourceFileName : string
	val moduleDataLayout : string
	val moduleTargetTriple : string option
	val moduleDefinations : string list
end

