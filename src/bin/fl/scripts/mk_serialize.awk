BEGIN	    {	dcnt = 0; }

/typedef struct [a-zA-Z][a-zA-Z0-9_]* \{/   {
    rec_name = $3;
    getline;
    i = 0;
    while( $1 != "}" ) {
	type[i] = $1;
	split($2, n, ";");
	split(n[1], nn, ":");
	name[i] = nn[1];
	i++;
	getline;
    }
    #
    #   Write functions
    #
    fwd_decl[dcnt++] = sprintf("static void write_%s(FILE *fp, pointer p)", rec_name);
    printf("static void\n");
    printf("write_%s(FILE *fp, pointer p)\n", rec_name);
    printf("{\n");
    printf("    %s *vp = (%s *) p;\n", rec_name, rec_name);
    for(j = 0; j < i; j++) {
	switch( type[j] ) {
	    case "int": {
		printf("    Write_int(fp, vp->%s);\n",name[j]);
		break;
	    }
	    case "unint": {
		printf("    Write_int(fp, vp->%s);\n",name[j]);
		break;
	    }
	    case "string": {
		printf("    Write_string(fp, vp->%s);\n",name[j]);
		break;
	    }
	    case "hash_record": {
		printf("    Write_hash_tbl(fp, vp->%s,\n", name[j]);
		printf("                   // void (*write_hash_key)(FILE *fp, pointer key),\n");
		printf("                   // void (*write_hash_data)(FILE *fp, pointer key)\n");
		printf("                  );\n");
		break;
	    }
	    case "rec_mgr": {
		
		loc = match(name[j], "_rec_mgr");
		if( loc == 0 ) {
		    printf("    Write_mgr(fp, vp->%s,\n", name[j]);
		    printf("              //void (*write_rec)(FILE *fp, pointer p))\n");
		    printf("             );\n");
		    break;
		} else {
		    rtype = substr(name[j],1, length(name[j])-8);
		    printf("    Write_mgr(fp, vp->%s, write_%s_rec);\n", name[j], rtype)
		    break;
		}
	    }
	    case "buffer": {
		
		printf("    Write_buffer(fp, vp->%s,\n", name[j]);
		printf("              //void (*write_item)(FILE *fp, pointer p))\n");
		printf("             );\n");
		break;
	    }
	    default: {
		printf("    Write_pointer(fp, vp->%s);\n",name[j]);
		break;
	    }
	}
    }
    printf("}\n\n");

    #
    #   Read functions
    #
    fwd_decl[dcnt++] = sprintf("static void read_%s(FILE *fp, pointer p)", rec_name);
    printf("static void\n");
    printf("read_%s(FILE *fp, pointer p)\n", rec_name);
    printf("{\n");
    printf("    %s *vp = (%s *) p;\n", rec_name, rec_name);
    for(j = 0; j < i; j++) {
	switch( type[j] ) {
	    case "int": {
		printf("    vp->%s = Read_int(fp);\n", name[j]);
		break;
	    }
	    case "unint": {
		printf("    vp->%s = Read_int(fp);\n", name[j]);
		break;
	    }
	    case "string": {
		printf("    vp->%s = Read_string(fp);\n", name[j]);
		break;
	    }
	    case "hash_record": {
		printf("    Read_hash_tbl(fp, &(vp->%s),\n", name[j]);
		printf("                   // void (*read_hash_key)(FILE *fp),\n");
		printf("                   // void (*read_hash_data)(FILE *fp)\n");
		printf("                  );\n");
		break;
	    }
	    case "rec_mgr": {
		loc = match(name[j], "_rec_mgr");
		if( loc == 0 ) {
		    printf("    Read_mgr(fp, &(vp->%s),\n", name[j]);
		    printf("              //void (*read_rec)(FILE *fp, pointer p))\n");
		    printf("             );\n");
		    break;
		} else {
		    rtype = substr(name[j],1, length(name[j])-8);
		    printf("    Read_mgr(fp, &(vp->%s), read_%s_rec);\n", name[j], rtype)
		    break;
		}
	    }
	    case "buffer": {
		printf("    Read_buffer(fp, &(vp->%s),\n", name[j]);
		printf("              //void (*read_item)(FILE *fp, pointer p))\n");
		printf("             );\n");
		break;
	    }
	    default: {
		printf("    vp->%s = Read_pointer(fp);\n", name[j]);
		break;
	    }
	}
    }
    printf("}\n\n");

    #
    #   Fix functions
    #
    fwd_decl[dcnt++] = sprintf("static void fix_%s_ptrs(pointer p)", rec_name);
    printf("static void\n");
    printf("fix_%s_ptrs(pointer p)\n", rec_name);
    printf("{\n");
    printf("    %s *vp = (%s *) p;\n", rec_name, rec_name);
    for(j = 0; j < i; j++) {
	switch( type[j] ) {
	    case "int": { break; }
	    case "unint": { break; }
	    case "string": { break; }
	    case "hash_record": {
		printf("    Fix_hash_tbl_ptrs(&(vp->%s),\n", name[j]);
		printf("                   // key_is_ptr,,\n");
		printf("                   // data_is_ptr\n");
		printf("                  );\n");
		break;
	    }
	    case "rec_mgr": {
		loc = match(name[j], "_rec_mgr");
		if( loc == 0 ) {
		    printf("    Fix_mgr_ptrs(fp, &(vp->%s),\n", name[j]);
		    printf("              //void (*update_rec_ptrs)(pointer p))\n");
		    printf("             );\n");
		    break;
		} else {
		    rtype = substr(name[j],1, length(name[j])-8);
		    printf("    Fix_mgr_ptrs(&(vp->%s),fix_%s_rec_ptrs);\n", name[j], rtype)
		    break;
		}
	    }
	    case "buffer": {
		printf("    Fix_buf_ptrs(&(vp->%s),\n", name[j]);
		printf("              //void (*update_item_ptrs)(pointer p))\n");
		printf("             );\n");
		break;
	    }
	    default: {
		printf("    vp->%s = Old2new(vp->%s);\n", name[j], name[j]);
		break;
	    }
	}
    }
    printf("}\n\n");

}

END	{
	    for(i = 0; i < dcnt; i++) {
		printf("%s;\n", fwd_decl[i]);
	    }
	}
