use crate::datastructures::arena::DroplessArena;
use crate::sea_of_nodes::ir_printer;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::tests::evaluator::{evaluate, Object};
use crate::sea_of_nodes::types::Types;

/// This used to crash with index out of bounds in the graph visualizer,
/// because the parse_while changed the current scope but not the x_scope.
#[test]
fn test_xscope_after_while() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("while(0)break;#showGraph;", &types);
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "return 0;");
}

#[test]
fn test_eval_negate_overflow() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("return -arg;", &types);
    let stop = parser.parse().unwrap();

    assert_eq!("return (-arg);", parser.print(stop));

    let nodes = parser.nodes;
    assert_eq!(
        Object::Long(-1),
        evaluate(&nodes, stop, Some(1), None).object
    );
    assert_eq!(
        Object::Long(i64::MIN),
        evaluate(&nodes, stop, Some(i64::MIN), None).object
    );
}

/// `force_exit` on loops doesn't set types on the newly created nodes which caused an unwrap in `_sched_late`
#[test]
fn gcm_unwrap_panic_bug() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("while(1) {};", &types);
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();
}

/// based on `chapter17::sieve_of_eratosthenes`
#[test]
fn test_visualizer_and_pretty_printer() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
var ary = new bool[arg], primes = new int[arg];
var nprimes=0, p=0;
// Find primes while p^2 < arg
for( p=2; p*p < arg; p++ ) {
    // skip marked non-primes
    while( ary[p] ) p++;
    // p is now a prime
    primes[nprimes++] = p;
    // Mark out the rest non-primes
    for( int i = p + p; i < ary#; i += p )
        ary[i] = true;
}
// Now just collect the remaining primes, no more marking
for( ; p < arg; p++ )
    if( !ary[p] )
        primes[nprimes++] = p;
// Copy/shrink the result array
var !rez = new int[nprimes];
for( int j=0; j<nprimes; j++ )
    rez[j] = primes[j];
return rez;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return [int];");

    // The java output with nodes sorted by their id for a consistent iteration order.
    let expected_graphviz = r##"
    digraph chapter11 {
/*
var ary = new bool[arg], primes = new int[arg];
var nprimes=0, p=0;
// Find primes while p^2 < arg
for( p=2; p*p < arg; p++ ) {
    // skip marked non-primes
    while( ary[p] ) p++;
    // p is now a prime
    primes[nprimes++] = p;
    // Mark out the rest non-primes
    for( int i = p + p; i < ary#; i += p )
        ary[i] = true;
}
// Now just collect the remaining primes, no more marking
for( ; p < arg; p++ )
    if( !ary[p] )
        primes[nprimes++] = p;
// Copy/shrink the result array
var !rez = new int[nprimes];
for( int j=0; j<nprimes; j++ )
    rez[j] = primes[j];
return rez;

*/
	rankdir=BT;
	concentrate="true";
	compound="true";
	subgraph cluster_Nodes {
		Start2 [ shape=plaintext label=<
			<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
			<TR><TD BGCOLOR="yellow">Start</TD></TR>
			<TR><TD>
				<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0">
				<TR><TD PORT="p0" BGCOLOR="yellow">$ctrl</TD><TD PORT="p1">$mem</TD><TD PORT="p2">arg</TD></TR>
				</TABLE>
			</TD></TR>
			</TABLE>>
		];
		Stop3 [ shape=box style=filled fillcolor=yellow label="Stop" ];
		Con_4 [ label="#0" ];
		Con_10 [ label="#8" ];
		Add13 [ label="+" ];
		new_ary_bool15 [ shape=plaintext label=<
			<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
			<TR><TD BGCOLOR="yellow">ary_bool</TD></TR>
			<TR><TD>
				<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0">
				<TR><TD PORT="p1">[bool]</TD><TD PORT="p2">$2</TD><TD PORT="p3">$3</TD></TR>
				</TABLE>
			</TD></TR>
			</TABLE>>
		];
		Con_20 [ label="#3" ];
		Shl21 [ label="<<" ];
		Add22 [ label="+" ];
		new_ary_int24 [ shape=plaintext label=<
			<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
			<TR><TD BGCOLOR="yellow">ary_int</TD></TR>
			<TR><TD>
				<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0">
				<TR><TD PORT="p1">[int]</TD><TD PORT="p2">$4</TD><TD PORT="p3">$5</TD></TR>
				</TABLE>
			</TD></TR>
			</TABLE>>
		];
		Con_30 [ label="#2" ];
		Loop31 [ shape=box style=filled fillcolor=yellow label="Loop" ];
		Phi_p34 [ style=filled fillcolor=lightyellow label="&phi;_p" ];
		Mul35 [ label="*" ];
		LT36 [ label="<" ];
		If38 [ shape=plaintext label=<
			<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
			<TR><TD BGCOLOR="yellow">If</TD></TR>
			<TR><TD>
				<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0">
				<TR><TD PORT="p0" BGCOLOR="yellow">True</TD><TD PORT="p1" BGCOLOR="yellow">False</TD></TR>
				</TABLE>
			</TD></TR>
			</TABLE>>
		];
		Con_45 [ label="#1" ];
		Loop50 [ shape=box style=filled fillcolor=yellow label="Loop" ];
		Phi_p56 [ style=filled fillcolor=lightyellow label="&phi;_p" ];
		Add59 [ label="+" ];
		Phi_360 [ style=filled fillcolor=lightyellow label="&phi;_$3" ];
		ld_ary62 [ label=".[]" ];
		If63 [ shape=plaintext label=<
			<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
			<TR><TD BGCOLOR="yellow">If</TD></TR>
			<TR><TD>
				<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0">
				<TR><TD PORT="p0" BGCOLOR="yellow">True</TD><TD PORT="p1" BGCOLOR="yellow">False</TD></TR>
				</TABLE>
			</TD></TR>
			</TABLE>>
		];
		Add74 [ label="+" ];
		Phi_nprimes77 [ style=filled fillcolor=lightyellow label="&phi;_nprimes" ];
		Add79 [ label="+" ];
		Shl81 [ label="<<" ];
		Add82 [ label="+" ];
		Phi_583 [ style=filled fillcolor=lightyellow label="&phi;_$5" ];
		st_ary84 [ label=".[]=" ];
		Mul87 [ label="*" ];
		Loop88 [ shape=box style=filled fillcolor=yellow label="Loop" ];
		Phi_i91 [ style=filled fillcolor=lightyellow label="&phi;_i" ];
		LT92 [ label="<" ];
		If98 [ shape=plaintext label=<
			<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
			<TR><TD BGCOLOR="yellow">If</TD></TR>
			<TR><TD>
				<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0">
				<TR><TD PORT="p0" BGCOLOR="yellow">True</TD><TD PORT="p1" BGCOLOR="yellow">False</TD></TR>
				</TABLE>
			</TD></TR>
			</TABLE>>
		];
		Add113 [ label="+" ];
		Phi_3115 [ style=filled fillcolor=lightyellow label="&phi;_$3" ];
		st_ary116 [ label=".[]=" ];
		Add117 [ label="+" ];
		Loop121 [ shape=box style=filled fillcolor=yellow label="Loop" ];
		Phi_p124 [ style=filled fillcolor=lightyellow label="&phi;_p" ];
		LT125 [ label="<" ];
		If127 [ shape=plaintext label=<
			<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
			<TR><TD BGCOLOR="yellow">If</TD></TR>
			<TR><TD>
				<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0">
				<TR><TD PORT="p0" BGCOLOR="yellow">True</TD><TD PORT="p1" BGCOLOR="yellow">False</TD></TR>
				</TABLE>
			</TD></TR>
			</TABLE>>
		];
		Add143 [ label="+" ];
		ld_ary145 [ label=".[]" ];
		If149 [ shape=plaintext label=<
			<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
			<TR><TD BGCOLOR="yellow">If</TD></TR>
			<TR><TD>
				<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0">
				<TR><TD PORT="p1" BGCOLOR="yellow">False</TD><TD PORT="p0" BGCOLOR="yellow">True</TD></TR>
				</TABLE>
			</TD></TR>
			</TABLE>>
		];
		Phi_nprimes162 [ style=filled fillcolor=lightyellow label="&phi;_nprimes" ];
		Add164 [ label="+" ];
		Shl166 [ label="<<" ];
		Add167 [ label="+" ];
		Phi_5168 [ style=filled fillcolor=lightyellow label="&phi;_$5" ];
		st_ary169 [ label=".[]=" ];
		Region170 [ shape=box style=filled fillcolor=yellow label="Region" ];
		Phi_5171 [ style=filled fillcolor=lightyellow label="&phi;_$5" ];
		Phi_nprimes173 [ style=filled fillcolor=lightyellow label="&phi;_nprimes" ];
		Add175 [ label="+" ];
		new_ary_int181 [ shape=plaintext label=<
			<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
			<TR><TD BGCOLOR="yellow">ary_int</TD></TR>
			<TR><TD>
				<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0">
				<TR><TD PORT="p1">[int]</TD><TD PORT="p2">$4</TD><TD PORT="p3">$5</TD></TR>
				</TABLE>
			</TD></TR>
			</TABLE>>
		];
		Loop186 [ shape=box style=filled fillcolor=yellow label="Loop" ];
		Phi_j189 [ style=filled fillcolor=lightyellow label="&phi;_j" ];
		LT190 [ label="<" ];
		If192 [ shape=plaintext label=<
			<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
			<TR><TD BGCOLOR="yellow">If</TD></TR>
			<TR><TD>
				<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0">
				<TR><TD PORT="p0" BGCOLOR="yellow">True</TD><TD PORT="p1" BGCOLOR="yellow">False</TD></TR>
				</TABLE>
			</TD></TR>
			</TABLE>>
		];
		Shl207 [ label="<<" ];
		Add208 [ label="+" ];
		Phi_5214 [ style=filled fillcolor=lightyellow label="&phi;_$5" ];
		ld_ary215 [ label=".[]" ];
		st_ary216 [ label=".[]=" ];
		Add218 [ label="+" ];
		Return219 [ shape=box style=filled fillcolor=yellow label="Return" ];
		{ rank=same; Start;}
		{ rank=same; Loop;Phi_360;Phi_p34;Phi_583;Phi_nprimes77;}
		{ rank=same; Loop;Phi_p56;}
		{ rank=same; Loop;Phi_3115;Phi_i91;}
		{ rank=same; Loop;Phi_5168;Phi_p124;Phi_nprimes162;}
		{ rank=same; Region;Phi_nprimes173;Phi_5171;}
		{ rank=same; Loop;Phi_5214;Phi_j189;}
	}
	edge [ fontname=Helvetica, fontsize=8 ];
	Stop3 -> Return219[taillabel=0 color=red];
	Add13 -> Start2:p0[taillabel=0 color=red];
	Add13 -> Start2:p2[taillabel=1];
	Add13 -> Con_10[taillabel=2];
	new_ary_bool15 -> Start2:p0[taillabel=0 color=green];
	new_ary_bool15 -> Add13[taillabel=1 color=green];
	new_ary_bool15 -> Start2:p1[taillabel=2 color=green];
	new_ary_bool15 -> Start2:p1[taillabel=3 color=green];
	new_ary_bool15 -> Start2:p2[taillabel=4 color=green];
	new_ary_bool15 -> Con_4[taillabel=5 color=green];
	Shl21 -> Start2:p0[taillabel=0 color=red];
	Shl21 -> Start2:p2[taillabel=1];
	Shl21 -> Con_20[taillabel=2];
	Add22 -> Start2:p0[taillabel=0 color=red];
	Add22 -> Shl21[taillabel=1];
	Add22 -> Con_10[taillabel=2];
	new_ary_int24 -> Start2:p0[taillabel=0 color=green];
	new_ary_int24 -> Add22[taillabel=1 color=green];
	new_ary_int24 -> Start2:p1[taillabel=2 color=green];
	new_ary_int24 -> Start2:p1[taillabel=3 color=green];
	new_ary_int24 -> Start2:p2[taillabel=4 color=green];
	new_ary_int24 -> Con_4[taillabel=5 color=green];
	Loop31 -> Start2:p0[taillabel=1 color=red];
	Loop31 -> If98:p1[taillabel=2 color=red constraint=false];
	Phi_p34 -> Loop31 [style=dotted taillabel=0];
	Phi_p34 -> Con_30[taillabel=1];
	Phi_p34 -> Add74[taillabel=2 constraint=false];
	Mul35 -> Loop31[taillabel=0 color=red];
	Mul35 -> Phi_p34[taillabel=1];
	Mul35 -> Phi_p34[taillabel=2];
	LT36 -> Loop31[taillabel=0 color=red];
	LT36 -> Mul35[taillabel=1];
	LT36 -> Start2:p2[taillabel=2];
	If38 -> Loop31[taillabel=0 color=red];
	If38 -> LT36[taillabel=1];
	Loop50 -> If38:p0[taillabel=1 color=red];
	Loop50 -> If63:p0[taillabel=2 color=red constraint=false];
	Phi_p56 -> Loop50 [style=dotted taillabel=0];
	Phi_p56 -> Phi_p34[taillabel=1];
	Phi_p56 -> Add74[taillabel=2 constraint=false];
	Add59 -> Loop50[taillabel=0 color=red];
	Add59 -> Phi_p56[taillabel=1];
	Add59 -> Con_10[taillabel=2];
	Phi_360 -> Loop31 [style=dotted taillabel=0];
	Phi_360 -> new_ary_bool15:p3[taillabel=1 color=blue];
	Phi_360 -> Phi_3115[taillabel=2 color=blue constraint=false];
	ld_ary62 -> Loop50[taillabel=0 color=red];
	ld_ary62 -> Phi_360[taillabel=1 color=blue];
	ld_ary62 -> new_ary_bool15:p1[taillabel=2];
	ld_ary62 -> Add59[taillabel=3];
	If63 -> Loop50[taillabel=0 color=red];
	If63 -> ld_ary62[taillabel=1];
	Add74 -> Loop50[taillabel=0 color=red];
	Add74 -> Phi_p56[taillabel=1];
	Add74 -> Con_45[taillabel=2];
	Phi_nprimes77 -> Loop31 [style=dotted taillabel=0];
	Phi_nprimes77 -> Con_4[taillabel=1];
	Phi_nprimes77 -> Add79[taillabel=2 constraint=false];
	Add79 -> If98:p1[taillabel=0 color=red];
	Add79 -> Phi_nprimes77[taillabel=1];
	Add79 -> Con_45[taillabel=2];
	Shl81 -> If98:p1[taillabel=0 color=red];
	Shl81 -> Phi_nprimes77[taillabel=1];
	Shl81 -> Con_20[taillabel=2];
	Add82 -> If98:p1[taillabel=0 color=red];
	Add82 -> Shl81[taillabel=1];
	Add82 -> Con_10[taillabel=2];
	Phi_583 -> Loop31 [style=dotted taillabel=0];
	Phi_583 -> new_ary_int24:p3[taillabel=1 color=blue];
	Phi_583 -> st_ary84[taillabel=2 color=blue constraint=false];
	st_ary84 -> If98:p1[taillabel=0 color=red];
	st_ary84 -> Phi_583[taillabel=1 color=blue];
	st_ary84 -> new_ary_int24:p1[taillabel=2];
	st_ary84 -> Add82[taillabel=3];
	st_ary84 -> Phi_p56[taillabel=4];
	Mul87 -> If63:p1[taillabel=0 color=red];
	Mul87 -> Phi_p56[taillabel=1];
	Mul87 -> Con_30[taillabel=2];
	Loop88 -> If63:p1[taillabel=1 color=red];
	Loop88 -> If98:p0[taillabel=2 color=red constraint=false];
	Phi_i91 -> Loop88 [style=dotted taillabel=0];
	Phi_i91 -> Mul87[taillabel=1];
	Phi_i91 -> Add117[taillabel=2 constraint=false];
	LT92 -> Loop88[taillabel=0 color=red];
	LT92 -> Phi_i91[taillabel=1];
	LT92 -> Start2:p2[taillabel=2];
	If98 -> Loop88[taillabel=0 color=red];
	If98 -> LT92[taillabel=1];
	Add113 -> If98:p0[taillabel=0 color=red];
	Add113 -> Phi_i91[taillabel=1];
	Add113 -> Con_10[taillabel=2];
	Phi_3115 -> Loop88 [style=dotted taillabel=0];
	Phi_3115 -> Phi_360[taillabel=1 color=blue];
	Phi_3115 -> st_ary116[taillabel=2 color=blue constraint=false];
	st_ary116 -> If98:p0[taillabel=0 color=red];
	st_ary116 -> Phi_3115[taillabel=1 color=blue];
	st_ary116 -> new_ary_bool15:p1[taillabel=2];
	st_ary116 -> Add113[taillabel=3];
	st_ary116 -> Con_45[taillabel=4];
	Add117 -> If98:p0[taillabel=0 color=red];
	Add117 -> Phi_i91[taillabel=1];
	Add117 -> Phi_p56[taillabel=2];
	Loop121 -> If38:p1[taillabel=1 color=red];
	Loop121 -> Region170[taillabel=2 color=red constraint=false];
	Phi_p124 -> Loop121 [style=dotted taillabel=0];
	Phi_p124 -> Phi_p34[taillabel=1];
	Phi_p124 -> Add175[taillabel=2 constraint=false];
	LT125 -> Loop121[taillabel=0 color=red];
	LT125 -> Phi_p124[taillabel=1];
	LT125 -> Start2:p2[taillabel=2];
	If127 -> Loop121[taillabel=0 color=red];
	If127 -> LT125[taillabel=1];
	Add143 -> If127:p0[taillabel=0 color=red];
	Add143 -> Phi_p124[taillabel=1];
	Add143 -> Con_10[taillabel=2];
	ld_ary145 -> If127:p0[taillabel=0 color=red];
	ld_ary145 -> Phi_360[taillabel=1 color=blue];
	ld_ary145 -> new_ary_bool15:p1[taillabel=2];
	ld_ary145 -> Add143[taillabel=3];
	If149 -> If127:p0[taillabel=0 color=red];
	If149 -> ld_ary145[taillabel=1];
	Phi_nprimes162 -> Loop121 [style=dotted taillabel=0];
	Phi_nprimes162 -> Phi_nprimes77[taillabel=1];
	Phi_nprimes162 -> Phi_nprimes173[taillabel=2 constraint=false];
	Add164 -> If149:p1[taillabel=0 color=red];
	Add164 -> Phi_nprimes162[taillabel=1];
	Add164 -> Con_45[taillabel=2];
	Shl166 -> Loop121[taillabel=0 color=red];
	Shl166 -> Phi_nprimes162[taillabel=1];
	Shl166 -> Con_20[taillabel=2];
	Add167 -> Loop121[taillabel=0 color=red];
	Add167 -> Shl166[taillabel=1];
	Add167 -> Con_10[taillabel=2];
	Phi_5168 -> Loop121 [style=dotted taillabel=0];
	Phi_5168 -> Phi_583[taillabel=1 color=blue];
	Phi_5168 -> Phi_5171[taillabel=2 color=blue constraint=false];
	st_ary169 -> If149:p1[taillabel=0 color=red];
	st_ary169 -> Phi_5168[taillabel=1 color=blue];
	st_ary169 -> new_ary_int24:p1[taillabel=2];
	st_ary169 -> Add167[taillabel=3];
	st_ary169 -> Phi_p124[taillabel=4];
	Region170 -> If149:p1[taillabel=1 color=red];
	Region170 -> If149:p0[taillabel=2 color=red];
	Phi_5171 -> Region170 [style=dotted taillabel=0];
	Phi_5171 -> st_ary169[taillabel=1 color=blue];
	Phi_5171 -> Phi_5168[taillabel=2 color=blue constraint=false];
	Phi_nprimes173 -> Region170 [style=dotted taillabel=0];
	Phi_nprimes173 -> Add164[taillabel=1];
	Phi_nprimes173 -> Phi_nprimes162[taillabel=2 constraint=false];
	Add175 -> Region170[taillabel=0 color=red];
	Add175 -> Phi_p124[taillabel=1];
	Add175 -> Con_45[taillabel=2];
	new_ary_int181 -> If127:p1[taillabel=0 color=green];
	new_ary_int181 -> Add167[taillabel=1 color=green];
	new_ary_int181 -> new_ary_int24:p2[taillabel=2 color=green];
	new_ary_int181 -> Phi_5168[taillabel=3 color=green];
	new_ary_int181 -> Phi_nprimes162[taillabel=4 color=green];
	new_ary_int181 -> Con_4[taillabel=5 color=green];
	Loop186 -> If127:p1[taillabel=1 color=red];
	Loop186 -> If192:p0[taillabel=2 color=red constraint=false];
	Phi_j189 -> Loop186 [style=dotted taillabel=0];
	Phi_j189 -> Con_4[taillabel=1];
	Phi_j189 -> Add218[taillabel=2 constraint=false];
	LT190 -> Loop186[taillabel=0 color=red];
	LT190 -> Phi_j189[taillabel=1];
	LT190 -> Phi_nprimes162[taillabel=2];
	If192 -> Loop186[taillabel=0 color=red];
	If192 -> LT190[taillabel=1];
	Shl207 -> If192:p0[taillabel=0 color=red];
	Shl207 -> Phi_j189[taillabel=1];
	Shl207 -> Con_20[taillabel=2];
	Add208 -> If192:p0[taillabel=0 color=red];
	Add208 -> Shl207[taillabel=1];
	Add208 -> Con_10[taillabel=2];
	Phi_5214 -> Loop186 [style=dotted taillabel=0];
	Phi_5214 -> new_ary_int181:p3[taillabel=1 color=blue];
	Phi_5214 -> st_ary216[taillabel=2 color=blue constraint=false];
	ld_ary215 -> If192:p0[taillabel=0 color=red];
	ld_ary215 -> Phi_5214[taillabel=1 color=blue];
	ld_ary215 -> new_ary_int24:p1[taillabel=2];
	ld_ary215 -> Add208[taillabel=3];
	st_ary216 -> If192:p0[taillabel=0 color=red];
	st_ary216 -> Phi_5214[taillabel=1 color=blue];
	st_ary216 -> new_ary_int181:p1[taillabel=2];
	st_ary216 -> Add208[taillabel=3];
	st_ary216 -> ld_ary215[taillabel=4];
	Add218 -> If192:p0[taillabel=0 color=red];
	Add218 -> Phi_j189[taillabel=1];
	Add218 -> Con_45[taillabel=2];
	Return219 -> If192:p1[taillabel=0 color=red];
	Return219 -> new_ary_int181:p1[taillabel=1];
	Return219 -> new_ary_bool15:p2[taillabel=2 color=blue];
	Return219 -> Phi_360[taillabel=3 color=blue];
	Return219 -> new_ary_int181:p2[taillabel=4 color=blue];
	Return219 -> Phi_5214[taillabel=5 color=blue];
}
"##;
    let actual_graphviz = parser.generate_graph();
    fn strip(s: &str) -> &str {
        &s[s.find("rankdir=BT;").unwrap()..]
    }
    assert_eq!(strip(&actual_graphviz), strip(expected_graphviz));

    let expected_pretty_print_scheduled = r#"START:                            [[   ]]  
   9 arg        2                 [[    36   13   15   21   24  125   92  ]]  int
   8 $mem       2                 [[    24   15   15   24  ]]  MEM#BOT

L7:                               [[  START      ]]  
  30 #2         7                 [[    34   87  ]]  2
  21 Shl        7    9   20       [[    22       ]]  int
  20 #3         7                 [[    21   81  166  207  ]]  3
  22 Add        7   21   10       [[    24       ]]  int
  24 new_ary    7   22    8    8    9    4  [[    25   26   27  ]]  [  Ctrl, *[int], MEM#4:int, MEM#5:0]
  25 $4        24                 [[   181       ]]  MEM#4:int
  26 $5        24                 [[    83       ]]  MEM#5:0
  27 [int]     24                 [[    84  169  215  ]]  *[int]
  45 #1         7                 [[    74  116   79  164  175  218  ]]  1
  10 #8         7                 [[    13   22   59   82  113  143  167  208  ]]  8
  13 Add        7    9   10       [[    15       ]]  int
   4 #0         7                 [[  ____   15   24   77  189  181  ]]  0
  15 new_ary    7   13    8    8    9    4  [[    16   17   18  ]]  [  Ctrl, *[bool], MEM#2:int, MEM#3:0]
  16 $2        15                 [[   219       ]]  MEM#2:int
  17 $3        15                 [[    60       ]]  MEM#3:0
  18 [bool]    15                 [[   116   62  145  ]]  *[bool]

LOOP31:                           [[  L7        L100       ]]  
  60 Phi_$3    31   17  115       [[   145   62  115  219  ]]  MEM#BOT
  34 Phi_p     31   30   74       [[   124   56   35   35  ]]  int
  83 Phi_$5    31   26   84       [[   168   84  ]]  MEM#5:Bot
  77 Phi_npr   31    4   79       [[   162   79   81  ]]  int
  35 Mul       31   34   34       [[    36       ]]  int
  36 LT        31   35    9       [[    38       ]]  bool
  38 If        31   36            [[    39   40  ]]  [  Ctrl, Ctrl]

L40:                              [[  LOOP31     ]]  

L39:                              [[  LOOP31     ]]  

LOOP121:                          [[  L40       L170       ]]  
 168 Phi_$5   121   83  171       [[   181  169  171  ]]  MEM#BOT
 124 Phi_p    121   34  175       [[   169  175  125  143  ]]  int
 162 Phi_npr  121   77  173       [[   190  164  166  173  181  ]]  int
 125 LT       121  124    9       [[   127       ]]  bool
 166 Shl      121  162   20       [[   167       ]]  int
 127 If       121  125            [[   128  129  ]]  [  Ctrl, Ctrl]
 167 Add      121  166   10       [[   169  181  ]]  int

LOOP50:                           [[  L39       L64        ]]  
  56 Phi_p     50   34   74       [[   117   74   59   84   87  ]]  int
  59 Add       50   56   10       [[    62       ]]  int
  74 Add       50   56   45       [[    56   34  ]]  int
  62 ld_ary    50   60   18   59  [[    63       ]]  int
  63 If        50   62            [[    64   65  ]]  [  Ctrl, Ctrl]

L129:                             [[  LOOP121    ]]  
 181 new_ary  129  167   25  168  162    4  [[   182  183  184  ]]  [  Ctrl, *[int], MEM#4:int, MEM#5:Bot]
 182 $4       181                 [[   219       ]]  MEM#4:int
 183 $5       181                 [[   214       ]]  MEM#5:Bot
 184 [int]    181                 [[   216  219  ]]  *[int]

L65:                              [[  LOOP50     ]]  
  87 Mul       65   56   30       [[    91       ]]  int

L64:                              [[  LOOP50     ]]  

L128:                             [[  LOOP121    ]]  
 143 Add      128  124   10       [[   145       ]]  int
 145 ld_ary   128   60   18  143  [[   149       ]]  int
 149 If       128  145            [[   150  153  ]]  [  Ctrl, Ctrl]

LOOP186:                          [[  L129      L193       ]]  
 214 Phi_$5   186  183  216       [[   219  216  215  ]]  MEM#5:Bot
 189 Phi_j    186    4  218       [[   207  218  190  ]]  int
 190 LT       186  189  162       [[   192       ]]  bool
 192 If       186  190            [[   193  194  ]]  [  Ctrl, Ctrl]

LOOP88:                           [[  L65       L99        ]]  
 115 Phi_$3    88   60  116       [[    60  116  ]]  MEM#BOT
  91 Phi_i     88   87  117       [[   113  117   92  ]]  int
  92 LT        88   91    9       [[    98       ]]  bool
  98 If        88   92            [[    99  100  ]]  [  Ctrl, Ctrl]

L150:                             [[  L128       ]]  
 169 st_ary   150  168   27  167  124  [[   171  ]]  MEM#5:Bot
 164 Add      150  162   45       [[   173       ]]  int

L153:                             [[  L128       ]]  

L194:                             [[  LOOP186    ]]  
 219 Return   194  184   16   60  182  214  [[     3  ]]  [  Ctrl, *[int]]

L100:                             [[  LOOP88     ]]  
  81 Shl      100   77   20       [[    82       ]]  int
  82 Add      100   81   10       [[    84       ]]  int
  79 Add      100   77   45       [[    77       ]]  int
  84 st_ary   100   83   27   82   56  [[    83  ]]  MEM#5:Bot

L193:                             [[  LOOP186    ]]  
 218 Add      193  189   45       [[   189       ]]  int
 207 Shl      193  189   20       [[   208       ]]  int
 215 ld_ary   193  214   27  208  [[   216       ]]  int
 208 Add      193  207   10       [[   215  216  ]]  int
 216 st_ary   193  214  184  208  215  [[   214  ]]  MEM#5:Bot

L99:                              [[  LOOP88     ]]  
 117 Add       99   91   56       [[    91       ]]  int
 113 Add       99   91   10       [[   116       ]]  int
 116 st_ary    99  115   18  113   45  [[   115  ]]  MEM#3:Bot

L170:                             [[  L150      L153       ]]  
 173 Phi_npr  170  164  162       [[   162       ]]  int
 171 Phi_$5   170  169  168       [[   168       ]]  MEM#BOT
 175 Add      170  124   45       [[   124       ]]  int

L3:                               [[  L194       ]]  

"#;
    let actual_pretty_print_scheduled = ir_printer::pretty_print(stop, 1000, &parser.nodes);
    assert_eq!(
        actual_pretty_print_scheduled,
        expected_pretty_print_scheduled
    );
}
