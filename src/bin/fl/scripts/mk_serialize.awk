BEGIN	    {
		dcnt = 0; 
		top_level["fsm_ptr"] = 1;
		top_level["ste_ptr"] = 1;
		top_level["vstate_ptr"] = 1;
		top_level["idx_list_ptr"] = 1;
		top_level["ilist_ptr"] = 1;
		marked["int"] = 1;
		marked["unint"] = 1;
		marked["bool"] = 1;
		marked["string"] = 1;
		marked["hash_record"] = 1;
		marked["rec_mgr"] = 1;
		marked["buffer"] = 1;
		dont_process["ncomp_rec"] = 1;
		prelude["ste_rec"] = "    old_type = current_type;\n    current_type = vp->type;\n";
		postlude["ste_rec"] = "    current_type = old_type;\n";
	    }

function TR(name) {
    if( name == "string") { return "str"; }
    return name;
}

function trim_trailing_semi(s) {
    return( substr(s, 1, (length(s)-1)) );
}

function PW (tp,txt,   s) {
    s = woutput[tp];
    woutput[tp] = sprintf("%s%s", s, txt);
}

function PR1 (tp,txt,   s) {
    s = output1[tp];
    output1[tp] = sprintf("%s%s", s, txt);
}

function PR2 (tp,txt,   s) {
    s = output2[tp];
    output2[tp] = sprintf("%s%s", s, txt);
}

function PDECL (tp,txt,   s) {
    s = declarations[tp];
    declarations[tp] = sprintf("%s%s", s, txt);
}

function mark_depends(tp,     dep,dtps,cnt,i) {
    if( marked[tp] == 1 ) {
	return "";
    }
    marked[tp] = 1;
    dep = depends[tp];
    cnt = split(dep,dtps," ");
    for(i = 1; i <= cnt; i++) {
	mark_depends(dtps[i]);
    }
}

function process_enum(tp, choices,     elements, i) {
    gsub(",", "", choices);
    elements = split(choices, types, " ");
    enum_types[tp] = 1
    depends[tp] = ""
    # 
    # Enum write function
    #
    if( top_level[tp] == 1 ) { pre = ""; } else { pre = "static "; }
    fwd_wr_decl[tp] = sprintf("%svoid write_%s(FILE *fp, %s v)", pre, tp, tp);
    PW(tp,sprintf("%svoid\nwrite_%s(FILE *fp, %s t)\n{\n", pre, tp, tp));
    PW(tp,sprintf("    WR_DBG0(\"{ %s\");\n", tp));
    PW(tp,sprintf("    switch( t ) {\n")); 
    for(i = 1; i <= elements; i++) {
        PW(tp,sprintf("        case %s: { write_int(fp,%d); break; }\n", types[i], i));
    }
    PW(tp,sprintf("        default: DIE(\"Impossible\");\n    }\n"));
    PW(tp,sprintf("    END_DBG(\"} %s\");\n", tp));
    PW(tp,sprintf("}\n\n"));
    # 
    # Enum read function
    #
    fwd_rd_decl[tp] = sprintf("%svoid read_%s(FILE *fp, %s *resp)", pre,tp,tp);
    PR1(tp,sprintf("%svoid\nread_%s(FILE *fp, %s *resp)\n{\n", pre, tp, tp));
    PR1(tp,sprintf("    int i;\n"));
    PR1(tp,sprintf("    RD_DBG0(\"{ %s\");\n", tp));
    PR1(tp,sprintf("    read_int(fp, &i);\n"));
    PR1(tp,sprintf("    %s res;\n", tp));
    PR1(tp,sprintf("    switch( i ) {\n"));
    for(i = 1; i <= elements; i++) {
        PR1(tp,sprintf("        case %d: { res = %s; break; }\n", i, types[i]));
    }
    PR1(tp,sprintf("        default: DIE(\"Impossible\");\n    }\n"));
    PR1(tp,sprintf("    END_DBG(\"} %s\");\n", tp));
    PR1(tp,sprintf("    *resp = res;\n}\n\n"));
}

/typedef enum[ \t]+{.*}/ {
    etype = trim_trailing_semi($NF);
    split($0, t1, "{");
    split(t1[2], t2, "}");
    process_enum(etype, t2[1]);
}

/typedef enum[ \t]+{[ \t]*$/ {
    getline;
    choices = ""
    sep = "";
    while( match($0, "}") == 0 ) {
        split($0,ll,"=");
        choices = sprintf("%s%s%s", choices, sep, ll[1]);
        sep = " ";
        getline;
    }
    etype = trim_trailing_semi($NF);
    process_enum(etype, choices);
}

/typedef union[ \t]+{[ \t]*$/ {
    getline;
    choices = ""
    sep = "";
    while( match($0, "}") == 0 ) {
	alt_type = $1;
	alt_name = trim_trailing_semi($2);
        choices = sprintf("%s%s%s %s", choices, sep, alt_type, alt_name);
        sep = "|";
        getline;
    }
    utype = trim_trailing_semi($NF);
    union_types[utype] = choices
    depends[utype] = "";
}

/typedef struct [a-zA-Z][a-zA-Z0-9_]*[ \t]+\*[ \t]*[a-zA-Z][a-zA-Z0-9_]*/   {
	rec_name = $3;
	tp = rec_name;
	n = trim_trailing_semi($4);
	pn = substr(n, 2);
	depends[pn] = tp;
	ptr2rec[pn] = rec_name;
	if( top_level[pn] == 1 ) { pre = ""; } else { pre = "static "; }
	fwd_wr_decl[pn] = sprintf("%svoid write_%s(FILE *fp, %s p)",pre,pn,pn);
	PW(pn,sprintf("%svoid\nwrite_%s(FILE *fp, %s p)\n{\n", pre, pn, pn));
	PW(pn,sprintf("    WR_DBG0(\"{ %s\");\n", pn));
	PW(pn,sprintf("    if( write_pointer(fp, (pointer) p) ) {\n"));
	PW(pn,sprintf("        write_%s(fp, p);\n", rec_name));
	PW(pn,sprintf("    }\n"));
	PW(pn,sprintf("    END_DBG(\"} %s\");\n", pn));
	PW(pn,sprintf("}\n\n"));
	//
	loc = match($0,"ALLOCATE:");
	if( loc != 0 ) {
	    allocator = sprintf("(pointer) %s", substr($0,loc+10));
	} else {
	    allocator = sprintf("new_rec(RD_%s_mgrp)", rec_name);
	    rec_mgrs[pn] = sprintf("rec_mgr *RD_%s_mgrp = NULL;\n", rec_name);
	}
	fwd_rd_decl[pn] = sprintf("%svoid read_%s(FILE *fp, %s *pp)",pre,pn,pn);
	PR1(pn,sprintf("%svoid\nread_%s(FILE *fp, %s *pp)\n{\n", pre, pn, pn));
	PR1(pn,sprintf("    pointer oldp, newp;\n"));
	PR1(pn,sprintf("    RD_DBG0(\"{ %s\");\n", pn));
	PR1(pn,sprintf("    read_pointer(fp, &oldp);\n"));
	PR1(pn,sprintf("    if( oldp == NULL ) {\n"));
	PR1(pn,sprintf("        *pp = NULL;\n"));
	PR1(pn,sprintf("        END_DBG(\"} %s\");\n", pn));
	PR1(pn,sprintf("        return;\n"));
	PR1(pn,sprintf("    }\n"));
	PR1(pn,sprintf("    if( (newp = Old2new(oldp)) != NULL ) {\n"));
	PR1(pn,sprintf("        *pp = (%s) newp;\n", pn));
	PR1(pn,sprintf("    } else {\n"));
	PR1(pn,sprintf("        newp = %s;\n", allocator));
	PR1(pn,sprintf("        Insert_pointer_map(oldp, newp);\n"));
	PR1(pn,sprintf("        read_%s(fp, newp);\n", rec_name));
	PR1(pn,sprintf("	*pp = (%s) newp;\n", pn));
	PR1(pn,sprintf("    }\n"));
	PR1(pn,sprintf("    END_DBG(\"} %s\");\n", pn));
	PR1(pn,sprintf("}\n\n"));
	next;
    }

/typedef struct [a-zA-Z][a-zA-Z0-9_]*[ \t]+\{/   {
    rec_name = $3;
    tp = rec_name;
    depends[tp] = "";
    struct_names[rec_name] = 1
    getline;
    i = 0;
    while( $1 != "}" ) {
	type[i] = $1;
	depends[tp] = sprintf("%s %s", depends[tp], $1);
	field_cnt = split(trim_trailing_semi($2), nn, ":");
	name[i] = nn[1];
	if( field_cnt > 1 ) {
	    has_field[i] = 1;
	}
	loc = match($0,"TYPE:");
	if( loc != 0 ) {
	    content_tp = substr($0,loc+6);
	    content[i] = content_tp;
	    aloc = match(content_tp, "->");
	    if( aloc == 0 ) {
		depends[tp] = sprintf("%s %s", depends[tp], content_tp);
	    } else {
		split(content_tp, tts, " ");
		depends[tp] = sprintf("%s %s %s", depends[tp], tts[1], tts[3]);
	    }
	}
	loc = match($0,"SEL:");
	if( loc != 0 ) {
	    sel = substr($0, loc+5);
	    alts = split(sel, my_alts, " ");
	    if( substr(my_alts[1], 1, 2) == "->" ) {
		selection_var[i] = sprintf("vp%s", my_alts[1]);
	    } else {
		selection_var[i] = sprintf("%s /* TODO Ensure setting! */ ", my_alts[1]);
	    }
	    s = "";
	    for(j = 3; j <= alts; j++) {
		s = sprintf("%s %s", s, my_alts[j]);
	    }
	    selection_alts[i] = s;
	}
	i++;
	getline;
    }
    #
    #   Write functions
    #
    if( top_level[tp] == 1 ) { pre = ""; } else { pre = "static "; }
    fwd_wr_decl[tp] = sprintf("%svoid write_%s(FILE *fp, pointer p)", pre, rec_name);
    PW(tp,sprintf("%svoid\n", pre));
    PW(tp,sprintf("write_%s(FILE *fp, pointer p)\n", rec_name));
    PW(tp,sprintf("{\n"));
    PW(tp,sprintf("    %s *vp = (%s *) p;\n", rec_name, rec_name));
    PW(tp,sprintf("    WR_DBG0(\"{ %s\");\n", rec_name));
    if( prelude[tp] != "" ) {
	PW(tp,prelude[tp]);
    }
    for(j = 0; j < i; j++) {
	switch( type[j] ) {
	    case "int": {
		PW(tp,sprintf("    write_int(fp, vp->%s);\n",name[j]));
		break;
	    }
	    case "unint": {
		PW(tp,sprintf("    write_unint(fp, (int) vp->%s);\n",name[j]));
		break;
	    }
	    case "bool": {
		PW(tp,sprintf("    write_int(fp, (int) vp->%s);\n",name[j]));
		break;
	    }
	    case "string": {
		PW(tp,sprintf("    write_string(fp, vp->%s);\n",name[j]));
		break;
	    }
	    case "hash_record": {
		rtype = content[j]
		split(rtype, hts, " ");
		ktype = hts[1];
		dtype = hts[3];
		PW(tp, sprintf("    write_hash_tbl(fp, &(vp->%s), (void (*)(FILE *fp, pointer key)) write_%s, (void (*)(FILE *fp, pointer key)) write_%s);\n",
		       name[j], ktype, dtype));
		break;
	    }
	    case "rec_mgr": {
		rtype = content[j]
		PW(tp, sprintf("    write_mgr(fp, &(vp->%s), write_%s);\n", name[j], rtype));
		break;
	    }
	    case "buffer": {
		rtype = content[j]
		PW(tp,sprintf("    write_buf(fp, &(vp->%s), (void (*)(FILE *fp, pointer p)) write_%s);\n", name[j], rtype));
		break;
	    }
	    case "ustr_mgr": {
		PW(tp,sprintf("    write_ustr_mgr(fp, &(fp->%s));\n", name[j]));
		break;
	    }
	    default: {
		if( type[j] in enum_types ) {
		    PW(tp,sprintf("    write_%s(fp, vp->%s);\n", type[j], name[j]));
		} else if( type[j] in union_types ) {
		    if( selection_var[j] != "" ) {
			sel_var = selection_var[j];
		    } else {
			sel_var = " /* TODO */ ";
		    }
		    PW(tp,sprintf("    switch( %s ) {\n", sel_var));
		    choices = union_types[type[j]];
		    alts = split(choices, alt, "|");
		    split(selection_alts[j], sas, " ");
		    for(k = 1; k <= alts; k++) {
			split(alt[k], tt, " ");
			if( sas[k] != "" ) {
			    scase = sas[k];
			} else {
			    scase = " /* TODO */ ";
			}
			PW(tp,sprintf("      case %s : {\n\tint ch = %d;\n\twrite_int(fp,ch);\n\twrite_%s(fp, vp->%s.%s);\n\tbreak;\n      }\n", scase, k, tt[1], name[j], tt[2]));
		    }
		    PW(tp,sprintf("      default: { DIE(\"Shouldn't happen\"); }\n"));
		    PW(tp,sprintf("    }\n"));
		} else {
		    PW(tp,sprintf("    write_%s(fp, vp->%s);\n",type[j], name[j]));
		}
		break;
	    }
	}
    }
    if( postlude[tp] != "" ) {
	PW(tp,postlude[tp]);
    }
    PW(tp,sprintf("    END_DBG(\"} %s\");\n", rec_name));
    PW(tp,sprintf("}\n\n"));

    #
    #   Read functions
    #
    fwd_rd_decl[tp] = sprintf("%svoid read_%s(FILE *fp, pointer p)", pre, rec_name);
    PR1(tp,sprintf("%svoid\n", pre));
    PR1(tp,sprintf("read_%s(FILE *fp, pointer p)\n", rec_name));
    PR1(tp,sprintf("{\n"));
    PR1(tp,sprintf("    RD_DBG0(\"{ %s\");\n", rec_name));
    PR1(tp,sprintf("    %s *vp = (%s *) p;\n", rec_name, rec_name));
    for(j = 0; j < i; j++) {
	switch( type[j] ) {
	    case "int":
	    case "bool": {
		if( has_field[j] == 1 ) {
		    PR2(tp,sprintf("    { int i; read_int(fp, &i); vp->%s = i; }\n", name[j]));
		} else {
		    PR2(tp,sprintf("    read_int(fp, &(vp->%s));\n", name[j]));
		}
		break;
	    }
	    case "unint": {
		if( has_field[j] == 1 ) {
		    PR2(tp,sprintf("    { unint i; read_unint(fp, &i); vp->%s = i; }\n", name[j]));
		} else {
		    PR2(tp,sprintf("    read_unint(fp, &(vp->%s));\n", name[j]));
		}
		break;
	    }
	    case "string": {
		PR2(tp,sprintf("    read_string(fp, &(vp->%s));\n", name[j]));
		break;
	    }
	    case "hash_record": {
		rtype = content[j]
		split(rtype, hts, " ");
		ktype = hts[1];
		dtype = hts[3];
		PDECL(tp, sprintf("    create_hash(&(vp->%s), 100, %s_hash, %s_equ);\n", name[j], TR(ktype), TR(ktype)));
		PR2(tp, sprintf("    read_hash_tbl(fp, &(vp->%s), (void (*)(FILE *fp, pointer *pp)) read_%s, (void (*)(FILE *fp, pointer *pp)) read_%s);\n",
		       name[j], ktype, dtype));
		break;
	    }
	    case "rec_mgr": {
		rtype = content[j]
		PDECL(tp, sprintf("    new_mgr(&(vp->%s), sizeof(%s));\n", name[j], rtype));
		PDECL(tp, sprintf("    RD_%s_mgrp = &(vp->%s);\n", rtype, name[j]));
		PR2(tp, sprintf("    read_mgr(fp, &(vp->%s), read_%s);\n",
		       name[j], rtype));
		break;
	    }
	    case "buffer": {
		rtype = content[j]
		PDECL(tp, sprintf("    new_buf(&(vp->%s), 100, sizeof(%s));\n", name[j], rtype));
		PR2(tp, sprintf("    read_buf(fp, &(vp->%s), (void (*)(FILE *fp, pointer p)) read_%s);\n",
		       name[j], rtype));
		break;
	    }
	    case "ustr_mgr": {
		PR2(tp,sprintf("    read_ustr_mgr(fp, &(fp->%s));\n", name[j]));
		break;
	    }
	    default: {
		if( type[j] in enum_types ) {
		    PR2(tp,sprintf("    read_%s(fp, &(vp->%s));\n", type[j], name[j]));
		} else if( type[j] in union_types ) {
		    PR2(tp,sprintf("    {int ch;\n"));
		    PR2(tp,sprintf("    read_int(fp, &ch);\n"));
		    PR2(tp,sprintf("    switch( ch ) {\n"));
		    choices = union_types[type[j]];
		    alts = split(choices, alt, "|");
		    for(k = 1; k <= alts; k++) {
			split(alt[k], tt, " ");
			PR2(tp,sprintf("        case %d: { read_%s(fp, &(vp->%s.%s)); break; }\n", k, tt[1], name[j], tt[2]));
		    }
		    PR2(tp,sprintf("        default: { DIE(\"Shouldn't happen\"); }\n"));
		    PR2(tp,sprintf("    }}\n"));
		} else {
		    PR2(tp,sprintf("    read_%s(fp, &(vp->%s));\n", type[j], name[j]));
		}
		break;
	    }
	}
    }
    PR2(tp,sprintf("    END_DBG(\"} %s\");\n", rec_name));
    PR2(tp,sprintf("}\n"));
}

END	{
	    for(tp in top_level) {
		if( top_level[tp] == 1 ) { mark_depends(tp); }
	    }
	    printf("#if 0\n/* Copy to serialize.h */\n\n");
	    for(tp in top_level) {
		if( top_level[tp] == 1 ) {
		    if( fwd_wr_decl[tp] != "" ) {
			printf("%s;\n", fwd_wr_decl[tp]);
		    } else {
			printf("void write_%s(FILE *fp, %s p);\n", tp, tp);
		    }
		    printf("%s;\n", fwd_rd_decl[tp]);
		}
	    }
	    printf("\n#endif\n\n");

	    printf("/**************************************************/ \n");
	    printf("/***************** Local variables ***************/ \n");
	    printf("/**************************************************/ \n");
	    for(tp in output1) {
		if( (marked[tp] == 1) && (rec_mgrs[tp] != "") ) {
		    printf("static %s", rec_mgrs[tp]);
		}
	    }
	    printf("\n\n");

	    # Local function declarations
	    printf("/**************************************************/ \n");
	    printf("/******* Declarations of local functions **********/ \n");
	    printf("/**************************************************/ \n");
	    for(tp in woutput) {
		if( (marked[tp] == 1) && (top_level[tp] != 1) ) {
		    if( dont_process[tp] != 1 ) {
			if( fwd_wr_decl[tp] != "" ) {
			    printf("%s;\n", fwd_wr_decl[tp]);
			}
			printf("%s;\n", fwd_rd_decl[tp]);
		    }
		}
	    }
	    printf("\n\n");
	    # Global functions
	    for(tp in top_level) {
		if( top_level[tp] == 1 ) {
		    printf("\n// ------------ Serialization functions for %s ---------\n", tp)
		    printf("%s", woutput[tp]);
		    printf("%s", output1[tp]);
		    printf("%s", declarations[tp]);
		    printf("%s", output2[tp]);
		}
	    }
	    printf("\n/*************************************************/\n");
	    printf("/***************** Local functions ***************/\n");
	    printf("/*************************************************/\n\n");
	    for(tp in woutput) {
		if( (marked[tp] == 1) && (top_level[tp] != 1) ) {
		    if( dont_process[tp] == 1 ) { printf("#if 0\n"); }
		    printf("%s", woutput[tp]);
		    printf("%s", output1[tp]);
		    printf("%s", declarations[tp]);
		    printf("%s", output2[tp]);
		    if( dont_process[tp] == 1 ) { printf("#endif\n\n"); }
		}
	    }
	}
