
source /home/cseger/VossII/src/bin/fl/utils.tcl
set ::imagedir /home/cseger/VossII/IDVII/src/fl/gui/sch_draw/images
load /home/cseger/VossII/src/external/visualization/schematic_draw_module.so
source /home/cseger/VossII/IDVII/src/fl/gui/sch_draw/draw_sch.tcl
proc fl_is_vector {args} { return 0 }
proc fl_set_local_time {args} { return 0 }
proc fl_set_color_by_bdd_prefix {args} { return 0 }
proc fl_update_colors {args} { return 0 }
set w .ste_debug
create_ste_debugger $w
val {tw c} [get_new_sch_canvas $w 0]
proc clean_name {name} {
    if { [regexp "^{\(.*\)}$" $name dummy base] } {
        set name $base
    }
    return $name
}
proc fl_tag2vec {c s} { return $s; }
proc fl_vecs2tags {c s} { return $s; }
proc fl_is_vector_name { s } {
        if { [string first {[} $s] != -1 } {
            return 1;
        } else {
            return 0;
        }
}

create_sch
set tr_0 [add_sch_object LEAF {an000002} {draw_ifc_input {{alloc_zero} }} {}]
set tr_1 [add_sch_object LEAF {an000003} {draw_ifc_input {{arity_zero} }} {}]
set tr_2 [add_sch_object LEAF {an000004} {draw_ifc_input {{comb_ack} }} {}]
set tr_3 [add_sch_object LEAF {an000005} {draw_ifc_input {{enough_args} }} {}]
set tr_4 [add_sch_object LEAF {an000006} {draw_ifc_input {{is_apply} }} {}]
set tr_5 [add_sch_object LEAF {an000007} {draw_ifc_input {{is_comb} }} {}]
set tr_6 [add_sch_object LEAF {an000008} {draw_ifc_input {{is_prim} }} {}]
set tr_7 [add_sch_object LEAF {an000009} {draw_ifc_input {{last_alloc} }} {}]
set tr_8 [add_sch_object LEAF {an000010} {draw_ifc_input {{last_arg} }} {}]
set tr_9 [add_sch_object LEAF {an000011} {draw_ifc_input {{mem_ack} }} {}]
set tr_10 [add_sch_object LEAF {an000012} {draw_ifc_input {{prim_ack} }} {}]
set tr_11 [add_sch_object LEAF {an000013} {draw_ifc_input {{reducto} }} {}]
set tr_12 [add_sch_object LEAF {an000014} {draw_ifc_input {{stack_ack} }} {}]
set tr_13 [add_sch_object LEAF {an000015} {draw_ifc_input {{reset} }} {}]
set tr_14 [add_sch_object LEAF {an000016} {draw_ifc_input {{clk} }} {}]
set tr_15 [add_sch_object NODE {an000001} {draw_fsm {MyFSM_moore} /tmp/voss2_cseger_wiAWp8/fsm_draw_2Mxk87 {{S0 MyFSM__alloc} {S3 MyFSM__comb} {S9 MyFSM__error_pop} {S12 MyFSM__handle_swap} {S13 MyFSM__idle} {S17 MyFSM__pop} {S27 MyFSM__prim} {S28 MyFSM__push_node} {S29 MyFSM__read_gp} {S31 MyFSM__wait_alloc} {S32 MyFSM__wait_comb} {S33 MyFSM__wait_pop} {S34 MyFSM__wait_prim} {S35 MyFSM__wait_push_node} {S36 MyFSM__wait_read_gp} {S37 MyFSM__wait_wba} {S38 MyFSM__wait_wbr} {S40 MyFSM__wba} {S41 MyFSM__wbr}} {{i8&i6&i10&i3 {(last_alloc & (is_comb & (mem_ack & comb_ack)))}} {~i3&~(i8&i6&i10&i3)&i8&i10&i6 {(~(comb_ack) & (~((last_alloc & (is_comb & (mem_ack & comb_ack)))) & (last_alloc & (mem_ack & is_comb))))}} {~i6&~(i8&i6&i10&i3|~i3&i10&i8&i6)&i10&i8&i11 {(~(is_comb) & (~(((last_alloc & (is_comb & (mem_ack & comb_ack))) | (~(comb_ack) & (mem_ack & (last_alloc & is_comb))))) & (mem_ack & (last_alloc & prim_ack))))}} {~i6&~i11&~(i8&i6&i10&i3|~i3&i10&i8&i6|~i6&i8&i10&i11)&i10&i8 {(~(is_comb) & (~(prim_ack) & (~((((last_alloc & (is_comb & (mem_ack & comb_ack))) | (~(comb_ack) & (mem_ack & (last_alloc & is_comb)))) | (~(is_comb) & (last_alloc & (mem_ack & prim_ack))))) & (mem_ack & last_alloc))))}} {~i8&~(i8&i6&i10&i3|~i3&i10&i8&i6|~i6&i8&i10&i11|~i6&~i11&i10&i8)&i10 {(~(last_alloc) & (~(((((last_alloc & (is_comb & (mem_ack & comb_ack))) | (~(comb_ack) & (mem_ack & (last_alloc & is_comb)))) | (~(is_comb) & (last_alloc & (mem_ack & prim_ack)))) | (~(is_comb) & (~(prim_ack) & (mem_ack & last_alloc))))) & mem_ack))}} {~i8&~i10&~(i8&i6&i10&i3|~i3&i10&i8&i6|~i6&i8&i10&i11|~i6&~i11&i10&i8|~i8&i10)&i10 {(~(last_alloc) & (~(mem_ack) & (~((((((last_alloc & (is_comb & (mem_ack & comb_ack))) | (~(comb_ack) & (mem_ack & (last_alloc & is_comb)))) | (~(is_comb) & (last_alloc & (mem_ack & prim_ack)))) | (~(is_comb) & (~(prim_ack) & (mem_ack & last_alloc)))) | (~(last_alloc) & mem_ack))) & mem_ack)))}} {i1&i3&i10 {(alloc_zero & (comb_ack & mem_ack))}} {~i10&~(i1&i3&i10)&i3&i1 {(~(mem_ack) & (~((alloc_zero & (comb_ack & mem_ack))) & (comb_ack & alloc_zero)))}} {~i1&~(i1&i3&i10|~i10&i3&i1)&i3&i10 {(~(alloc_zero) & (~(((alloc_zero & (comb_ack & mem_ack)) | (~(mem_ack) & (comb_ack & alloc_zero)))) & (comb_ack & mem_ack)))}} {~i1&~i10&~(i1&i3&i10|~i10&i3&i1|~i1&i3&i10)&i3 {(~(alloc_zero) & (~(mem_ack) & (~((((alloc_zero & (comb_ack & mem_ack)) | (~(mem_ack) & (comb_ack & alloc_zero))) | (~(alloc_zero) & (comb_ack & mem_ack)))) & comb_ack)))}} {i12&i10 {(reducto & mem_ack)}} {~i10&~(i12&i10)&i12 {(~(mem_ack) & (~((reducto & mem_ack)) & reducto))}} {~(i12&i10|~i10&i12)&~i12 {(~(((reducto & mem_ack) | (~(mem_ack) & reducto))) & ~(reducto))}} {i1&i6&i13&i9&i3 {(alloc_zero & (is_comb & (stack_ack & (last_arg & comb_ack))))}} {~i3&~(i1&i6&i13&i9&i3)&i9&i1&i13&i6 {(~(comb_ack) & (~((alloc_zero & (is_comb & (stack_ack & (last_arg & comb_ack))))) & (last_arg & (alloc_zero & (stack_ack & is_comb)))))}} {~i6&~(i1&i6&i13&i9&i3|~i3&i9&i13&i1&i6)&i1&i13&i9&i11 {(~(is_comb) & (~(((alloc_zero & (is_comb & (stack_ack & (last_arg & comb_ack)))) | (~(comb_ack) & (last_arg & (stack_ack & (alloc_zero & is_comb)))))) & (alloc_zero & (stack_ack & (last_arg & prim_ack)))))}} {~i6&~i11&~(i1&i6&i13&i9&i3|~i3&i9&i13&i1&i6|~i6&i1&i9&i13&i11)&i13&i9&i1 {(~(is_comb) & (~(prim_ack) & (~((((alloc_zero & (is_comb & (stack_ack & (last_arg & comb_ack)))) | (~(comb_ack) & (last_arg & (stack_ack & (alloc_zero & is_comb))))) | (~(is_comb) & (alloc_zero & (last_arg & (stack_ack & prim_ack)))))) & (stack_ack & (last_arg & alloc_zero)))))}} {~i1&~(i1&i6&i13&i9&i3|~i3&i9&i13&i1&i6|~i6&i1&i9&i13&i11|~i6&~i11&i9&i13&i1)&i13&i9&i10 {(~(alloc_zero) & (~(((((alloc_zero & (is_comb & (stack_ack & (last_arg & comb_ack)))) | (~(comb_ack) & (last_arg & (stack_ack & (alloc_zero & is_comb))))) | (~(is_comb) & (alloc_zero & (last_arg & (stack_ack & prim_ack))))) | (~(is_comb) & (~(prim_ack) & (last_arg & (stack_ack & alloc_zero)))))) & (stack_ack & (last_arg & mem_ack))))}} {~i1&~i10&~(i1&i6&i13&i9&i3|~i3&i9&i13&i1&i6|~i6&i1&i9&i13&i11|~i6&~i11&i9&i13&i1|~i1&i9&i13&i10)&i13&i9 {(~(alloc_zero) & (~(mem_ack) & (~((((((alloc_zero & (is_comb & (stack_ack & (last_arg & comb_ack)))) | (~(comb_ack) & (last_arg & (stack_ack & (alloc_zero & is_comb))))) | (~(is_comb) & (alloc_zero & (last_arg & (stack_ack & prim_ack))))) | (~(is_comb) & (~(prim_ack) & (last_arg & (stack_ack & alloc_zero))))) | (~(alloc_zero) & (last_arg & (stack_ack & mem_ack))))) & (stack_ack & last_arg))))}} {~i9&~(i1&i6&i13&i9&i3|~i3&i9&i13&i1&i6|~i6&i1&i9&i13&i11|~i6&~i11&i9&i13&i1|~i1&i9&i13&i10|~i1&~i10&i13&i9)&i13 {(~(last_arg) & (~(((((((alloc_zero & (is_comb & (stack_ack & (last_arg & comb_ack)))) | (~(comb_ack) & (last_arg & (stack_ack & (alloc_zero & is_comb))))) | (~(is_comb) & (alloc_zero & (last_arg & (stack_ack & prim_ack))))) | (~(is_comb) & (~(prim_ack) & (last_arg & (stack_ack & alloc_zero))))) | (~(alloc_zero) & (last_arg & (stack_ack & mem_ack)))) | (~(alloc_zero) & (~(mem_ack) & (stack_ack & last_arg))))) & stack_ack))}} {~i9&~i13&~(i1&i6&i13&i9&i3|~i3&i9&i13&i1&i6|~i6&i1&i9&i13&i11|~i6&~i11&i9&i13&i1|~i1&i9&i13&i10|~i1&~i10&i13&i9|~i9&i13)&i13 {(~(last_arg) & (~(stack_ack) & (~((((((((alloc_zero & (is_comb & (stack_ack & (last_arg & comb_ack)))) | (~(comb_ack) & (last_arg & (stack_ack & (alloc_zero & is_comb))))) | (~(is_comb) & (alloc_zero & (last_arg & (stack_ack & prim_ack))))) | (~(is_comb) & (~(prim_ack) & (last_arg & (stack_ack & alloc_zero))))) | (~(alloc_zero) & (last_arg & (stack_ack & mem_ack)))) | (~(alloc_zero) & (~(mem_ack) & (stack_ack & last_arg)))) | (~(last_arg) & stack_ack))) & stack_ack)))}} {i1&i11&i10 {(alloc_zero & (prim_ack & mem_ack))}} {~i10&~(i1&i11&i10)&i11&i1 {(~(mem_ack) & (~((alloc_zero & (prim_ack & mem_ack))) & (prim_ack & alloc_zero)))}} {~i1&~(i1&i11&i10|~i10&i11&i1)&i11&i10 {(~(alloc_zero) & (~(((alloc_zero & (prim_ack & mem_ack)) | (~(mem_ack) & (prim_ack & alloc_zero)))) & (prim_ack & mem_ack)))}} {~i1&~i10&~(i1&i11&i10|~i10&i11&i1|~i1&i11&i10)&i11 {(~(alloc_zero) & (~(mem_ack) & (~((((alloc_zero & (prim_ack & mem_ack)) | (~(mem_ack) & (prim_ack & alloc_zero))) | (~(alloc_zero) & (prim_ack & mem_ack)))) & prim_ack)))}} {i13&i10 {(stack_ack & mem_ack)}} {~i10&~(i13&i10)&i13 {(~(mem_ack) & (~((stack_ack & mem_ack)) & stack_ack))}} {i5&i10&i13 {(is_apply & (mem_ack & stack_ack))}} {~i13&~(i5&i10&i13)&i10&i5 {(~(stack_ack) & (~((is_apply & (mem_ack & stack_ack))) & (mem_ack & is_apply)))}} {~i5&~(i5&i10&i13|~i13&i10&i5)&i1&(i6|i7)&i10&i2&i6&i3 {(~(is_apply) & (~(((is_apply & (mem_ack & stack_ack)) | (~(stack_ack) & (mem_ack & is_apply)))) & (alloc_zero & ((is_comb | is_prim) & (mem_ack & (arity_zero & (is_comb & comb_ack)))))))}} {~i5&~i3&~(i5&i10&i13|~i13&i10&i5|~i5&i6&i1&i2&(i6|i7)&i10&i3)&i1&i2&(i6|i7)&i10&i6 {(~(is_apply) & (~(comb_ack) & (~((((is_apply & (mem_ack & stack_ack)) | (~(stack_ack) & (mem_ack & is_apply))) | (~(is_apply) & (is_comb & (alloc_zero & (arity_zero & ((is_comb | is_prim) & (mem_ack & comb_ack)))))))) & (alloc_zero & (arity_zero & ((is_comb | is_prim) & (mem_ack & is_comb)))))))}} {~i5&~i6&~(i5&i10&i13|~i13&i10&i5|~i5&i6&i1&i2&(i6|i7)&i10&i3|~i5&~i3&i1&i2&(i6|i7)&i10&i6)&(i6|i7)&i1&i10&i2&i11 {(~(is_apply) & (~(is_comb) & (~(((((is_apply & (mem_ack & stack_ack)) | (~(stack_ack) & (mem_ack & is_apply))) | (~(is_apply) & (is_comb & (alloc_zero & (arity_zero & ((is_comb | is_prim) & (mem_ack & comb_ack))))))) | (~(is_apply) & (~(comb_ack) & (alloc_zero & (arity_zero & ((is_comb | is_prim) & (mem_ack & is_comb)))))))) & ((is_comb | is_prim) & (alloc_zero & (mem_ack & (arity_zero & prim_ack)))))))}} {~i5&~i6&~i11&~(i5&i10&i13|~i13&i10&i5|~i5&i6&i1&i2&(i6|i7)&i10&i3|~i5&~i3&i1&i2&(i6|i7)&i10&i6|~i5&~i6&(i6|i7)&i1&i10&i2&i11)&i2&(i6|i7)&i10&i1 {(~(is_apply) & (~(is_comb) & (~(prim_ack) & (~((((((is_apply & (mem_ack & stack_ack)) | (~(stack_ack) & (mem_ack & is_apply))) | (~(is_apply) & (is_comb & (alloc_zero & (arity_zero & ((is_comb | is_prim) & (mem_ack & comb_ack))))))) | (~(is_apply) & (~(comb_ack) & (alloc_zero & (arity_zero & ((is_comb | is_prim) & (mem_ack & is_comb))))))) | (~(is_apply) & (~(is_comb) & ((is_comb | is_prim) & (alloc_zero & (mem_ack & (arity_zero & prim_ack)))))))) & (arity_zero & ((is_comb | is_prim) & (mem_ack & alloc_zero)))))))}} {~i5&~i1&~(i5&i10&i13|~i13&i10&i5|~i5&i6&i1&i2&(i6|i7)&i10&i3|~i5&~i3&i1&i2&(i6|i7)&i10&i6|~i5&~i6&(i6|i7)&i1&i10&i2&i11|~i5&~i6&~i11&i2&i10&(i6|i7)&i1)&i10&(i6|i7)&i2 {(~(is_apply) & (~(alloc_zero) & (~(((((((is_apply & (mem_ack & stack_ack)) | (~(stack_ack) & (mem_ack & is_apply))) | (~(is_apply) & (is_comb & (alloc_zero & (arity_zero & ((is_comb | is_prim) & (mem_ack & comb_ack))))))) | (~(is_apply) & (~(comb_ack) & (alloc_zero & (arity_zero & ((is_comb | is_prim) & (mem_ack & is_comb))))))) | (~(is_apply) & (~(is_comb) & ((is_comb | is_prim) & (alloc_zero & (mem_ack & (arity_zero & prim_ack))))))) | (~(is_apply) & (~(is_comb) & (~(prim_ack) & (arity_zero & (mem_ack & ((is_comb | is_prim) & alloc_zero)))))))) & (mem_ack & ((is_comb | is_prim) & arity_zero)))))}} {~i5&~i1&~i10&~(i5&i10&i13|~i13&i10&i5|~i5&i6&i1&i2&(i6|i7)&i10&i3|~i5&~i3&i1&i2&(i6|i7)&i10&i6|~i5&~i6&(i6|i7)&i1&i10&i2&i11|~i5&~i6&~i11&i2&i10&(i6|i7)&i1|~i5&~i1&(i6|i7)&i10&i2)&i10&(i6|i7)&i2 {(~(is_apply) & (~(alloc_zero) & (~(mem_ack) & (~((((((((is_apply & (mem_ack & stack_ack)) | (~(stack_ack) & (mem_ack & is_apply))) | (~(is_apply) & (is_comb & (alloc_zero & (arity_zero & ((is_comb | is_prim) & (mem_ack & comb_ack))))))) | (~(is_apply) & (~(comb_ack) & (alloc_zero & (arity_zero & ((is_comb | is_prim) & (mem_ack & is_comb))))))) | (~(is_apply) & (~(is_comb) & ((is_comb | is_prim) & (alloc_zero & (mem_ack & (arity_zero & prim_ack))))))) | (~(is_apply) & (~(is_comb) & (~(prim_ack) & (arity_zero & (mem_ack & ((is_comb | is_prim) & alloc_zero))))))) | (~(is_apply) & (~(alloc_zero) & ((is_comb | is_prim) & (mem_ack & arity_zero)))))) & (mem_ack & ((is_comb | is_prim) & arity_zero))))))}} {~i5&~i2&~(i5&i10&i13|~i13&i10&i5|~i5&i6&i1&i2&(i6|i7)&i10&i3|~i5&~i3&i1&i2&(i6|i7)&i10&i6|~i5&~i6&(i6|i7)&i1&i10&i2&i11|~i5&~i6&~i11&i2&i10&(i6|i7)&i1|~i5&~i1&(i6|i7)&i10&i2|~i5&~i1&~i10&(i6|i7)&i10&i2)&(i6|i7)&i10&i4&i13 {(~(is_apply) & (~(arity_zero) & (~(((((((((is_apply & (mem_ack & stack_ack)) | (~(stack_ack) & (mem_ack & is_apply))) | (~(is_apply) & (is_comb & (alloc_zero & (arity_zero & ((is_comb | is_prim) & (mem_ack & comb_ack))))))) | (~(is_apply) & (~(comb_ack) & (alloc_zero & (arity_zero & ((is_comb | is_prim) & (mem_ack & is_comb))))))) | (~(is_apply) & (~(is_comb) & ((is_comb | is_prim) & (alloc_zero & (mem_ack & (arity_zero & prim_ack))))))) | (~(is_apply) & (~(is_comb) & (~(prim_ack) & (arity_zero & (mem_ack & ((is_comb | is_prim) & alloc_zero))))))) | (~(is_apply) & (~(alloc_zero) & ((is_comb | is_prim) & (mem_ack & arity_zero))))) | (~(is_apply) & (~(alloc_zero) & (~(mem_ack) & ((is_comb | is_prim) & (mem_ack & arity_zero))))))) & ((is_comb | is_prim) & (mem_ack & (enough_args & stack_ack))))))}} {~i5&~i2&~i13&~(i5&i10&i13|~i13&i10&i5|~i5&i6&i1&i2&(i6|i7)&i10&i3|~i5&~i3&i1&i2&(i6|i7)&i10&i6|~i5&~i6&(i6|i7)&i1&i10&i2&i11|~i5&~i6&~i11&i2&i10&(i6|i7)&i1|~i5&~i1&(i6|i7)&i10&i2|~i5&~i1&~i10&(i6|i7)&i10&i2|~i5&~i2&(i6|i7)&i4&i10&i13)&(i6|i7)&i10&i4 {(~(is_apply) & (~(arity_zero) & (~(stack_ack) & (~((((((((((is_apply & (mem_ack & stack_ack)) | (~(stack_ack) & (mem_ack & is_apply))) | (~(is_apply) & (is_comb & (alloc_zero & (arity_zero & ((is_comb | is_prim) & (mem_ack & comb_ack))))))) | (~(is_apply) & (~(comb_ack) & (alloc_zero & (arity_zero & ((is_comb | is_prim) & (mem_ack & is_comb))))))) | (~(is_apply) & (~(is_comb) & ((is_comb | is_prim) & (alloc_zero & (mem_ack & (arity_zero & prim_ack))))))) | (~(is_apply) & (~(is_comb) & (~(prim_ack) & (arity_zero & (mem_ack & ((is_comb | is_prim) & alloc_zero))))))) | (~(is_apply) & (~(alloc_zero) & ((is_comb | is_prim) & (mem_ack & arity_zero))))) | (~(is_apply) & (~(alloc_zero) & (~(mem_ack) & ((is_comb | is_prim) & (mem_ack & arity_zero)))))) | (~(is_apply) & (~(arity_zero) & ((is_comb | is_prim) & (enough_args & (mem_ack & stack_ack))))))) & ((is_comb | is_prim) & (mem_ack & enough_args))))))}} {~i5&~i2&~i4&~(i5&i10&i13|~i13&i10&i5|~i5&i6&i1&i2&(i6|i7)&i10&i3|~i5&~i3&i1&i2&(i6|i7)&i10&i6|~i5&~i6&(i6|i7)&i1&i10&i2&i11|~i5&~i6&~i11&i2&i10&(i6|i7)&i1|~i5&~i1&(i6|i7)&i10&i2|~i5&~i1&~i10&(i6|i7)&i10&i2|~i5&~i2&(i6|i7)&i4&i10&i13|~i5&~i2&~i13&i10&(i6|i7)&i4)&i10&(i6|i7) {(~(is_apply) & (~(arity_zero) & (~(enough_args) & (~(((((((((((is_apply & (mem_ack & stack_ack)) | (~(stack_ack) & (mem_ack & is_apply))) | (~(is_apply) & (is_comb & (alloc_zero & (arity_zero & ((is_comb | is_prim) & (mem_ack & comb_ack))))))) | (~(is_apply) & (~(comb_ack) & (alloc_zero & (arity_zero & ((is_comb | is_prim) & (mem_ack & is_comb))))))) | (~(is_apply) & (~(is_comb) & ((is_comb | is_prim) & (alloc_zero & (mem_ack & (arity_zero & prim_ack))))))) | (~(is_apply) & (~(is_comb) & (~(prim_ack) & (arity_zero & (mem_ack & ((is_comb | is_prim) & alloc_zero))))))) | (~(is_apply) & (~(alloc_zero) & ((is_comb | is_prim) & (mem_ack & arity_zero))))) | (~(is_apply) & (~(alloc_zero) & (~(mem_ack) & ((is_comb | is_prim) & (mem_ack & arity_zero)))))) | (~(is_apply) & (~(arity_zero) & ((is_comb | is_prim) & (enough_args & (mem_ack & stack_ack)))))) | (~(is_apply) & (~(arity_zero) & (~(stack_ack) & (mem_ack & ((is_comb | is_prim) & enough_args))))))) & (mem_ack & (is_comb | is_prim))))))}} {~i5&~(i6|i7)&~(i5&i10&i13|~i13&i10&i5|~i5&i6&i1&i2&(i6|i7)&i10&i3|~i5&~i3&i1&i2&(i6|i7)&i10&i6|~i5&~i6&(i6|i7)&i1&i10&i2&i11|~i5&~i6&~i11&i2&i10&(i6|i7)&i1|~i5&~i1&(i6|i7)&i10&i2|~i5&~i1&~i10&(i6|i7)&i10&i2|~i5&~i2&(i6|i7)&i4&i10&i13|~i5&~i2&~i13&i10&(i6|i7)&i4|~i5&~i2&~i4&i10&(i6|i7))&i10 {(~(is_apply) & (~((is_comb | is_prim)) & (~((((((((((((is_apply & (mem_ack & stack_ack)) | (~(stack_ack) & (mem_ack & is_apply))) | (~(is_apply) & (is_comb & (alloc_zero & (arity_zero & ((is_comb | is_prim) & (mem_ack & comb_ack))))))) | (~(is_apply) & (~(comb_ack) & (alloc_zero & (arity_zero & ((is_comb | is_prim) & (mem_ack & is_comb))))))) | (~(is_apply) & (~(is_comb) & ((is_comb | is_prim) & (alloc_zero & (mem_ack & (arity_zero & prim_ack))))))) | (~(is_apply) & (~(is_comb) & (~(prim_ack) & (arity_zero & (mem_ack & ((is_comb | is_prim) & alloc_zero))))))) | (~(is_apply) & (~(alloc_zero) & ((is_comb | is_prim) & (mem_ack & arity_zero))))) | (~(is_apply) & (~(alloc_zero) & (~(mem_ack) & ((is_comb | is_prim) & (mem_ack & arity_zero)))))) | (~(is_apply) & (~(arity_zero) & ((is_comb | is_prim) & (enough_args & (mem_ack & stack_ack)))))) | (~(is_apply) & (~(arity_zero) & (~(stack_ack) & (mem_ack & ((is_comb | is_prim) & enough_args)))))) | (~(is_apply) & (~(arity_zero) & (~(enough_args) & (mem_ack & (is_comb | is_prim))))))) & mem_ack)))}} {i10 {mem_ack}} {~i10 {~(mem_ack)}} {i3 {comb_ack}} {~i3 {~(comb_ack)}} {i13 {stack_ack}} {~i13 {~(stack_ack)}} {i11 {prim_ack}} {~i11 {~(prim_ack)}} {i13 {stack_ack}} {~i13 {~(stack_ack)}} {i10 {mem_ack}} {~i10 {~(mem_ack)}} {i10 {mem_ack}} {~i10 {~(mem_ack)}} {i10 {mem_ack}} {~i10 {~(mem_ack)}} {i10&i8 {(mem_ack & last_alloc)}} {~i10&~(i10&i8)&i10&i8 {(~(mem_ack) & (~((mem_ack & last_alloc)) & (mem_ack & last_alloc)))}} {~i8&~(i10&i8|~i10&i10&i8)&i10 {(~(last_alloc) & (~(((mem_ack & last_alloc) | (~(mem_ack) & (mem_ack & last_alloc)))) & mem_ack))}} {~i8&~i10&~(i10&i8|~i10&i10&i8|~i8&i10)&i10 {(~(last_alloc) & (~(mem_ack) & (~((((mem_ack & last_alloc) | (~(mem_ack) & (mem_ack & last_alloc))) | (~(last_alloc) & mem_ack))) & mem_ack)))}} {i10&i12 {(mem_ack & reducto)}} {~i10&~(i10&i12)&i10&i12 {(~(mem_ack) & (~((mem_ack & reducto)) & (mem_ack & reducto)))}} {~i12&~(i10&i12|~i10&i10&i12)&i10 {(~(reducto) & (~(((mem_ack & reducto) | (~(mem_ack) & (mem_ack & reducto)))) & mem_ack))}}} {{alloc_zero} {arity_zero} {comb_ack} {enough_args} {is_apply} {is_comb} {is_prim} {last_alloc} {last_arg} {mem_ack} {prim_ack} {reducto} {stack_ack} {reset} {clk}}} [list $tr_0 $tr_1 $tr_2 $tr_3 $tr_4 $tr_5 $tr_6 $tr_7 $tr_8 $tr_9 $tr_10 $tr_11 $tr_12 $tr_13 $tr_14]]
set tr_16 [add_sch_object NODE {an000000} {draw_predicate is_MyFSM__comb 1} [list $tr_15]]
set tr_17 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_18 [add_sch_object NODE {an000018} {draw_predicate is_MyFSM__push_node 1} [list $tr_17]]
set tr_19 [add_sch_object LEAF {an000014} {draw_repeat_nd} {}]
set tr_20 [add_sch_object NODE {an000017} {draw_and2} [list $tr_18 $tr_19]]
set tr_21 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_22 [add_sch_object NODE {an000021} {draw_predicate is_MyFSM__alloc 1} [list $tr_21]]
set tr_23 [add_sch_object LEAF {an000011} {draw_repeat_nd} {}]
set tr_24 [add_sch_object NODE {an000020} {draw_and2} [list $tr_22 $tr_23]]
set tr_25 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_26 [add_sch_object NODE {an000023} {draw_predicate is_MyFSM__wba 1} [list $tr_25]]
set tr_27 [add_sch_object LEAF {an000011} {draw_repeat_nd} {}]
set tr_28 [add_sch_object NODE {an000022} {draw_and2} [list $tr_26 $tr_27]]
set tr_29 [add_sch_object NODE {an000019} {draw_or2} [list $tr_24 $tr_28]]
set tr_30 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_31 [add_sch_object NODE {an000025} {draw_predicate is_MyFSM__pop 1} [list $tr_30]]
set tr_32 [add_sch_object LEAF {an000014} {draw_repeat_nd} {}]
set tr_33 [add_sch_object NODE {an000024} {draw_and2} [list $tr_31 $tr_32]]
set tr_34 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_35 [add_sch_object NODE {an000027} {draw_predicate is_MyFSM__alloc 1} [list $tr_34]]
set tr_36 [add_sch_object LEAF {an000011} {draw_repeat_nd} {}]
set tr_37 [add_sch_object NODE {an000026} {draw_and2} [list $tr_35 $tr_36]]
set tr_38 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_39 [add_sch_object NODE {an000030} {draw_predicate is_MyFSM__prim 1} [list $tr_38]]
set tr_40 [add_sch_object LEAF {an000012} {draw_repeat_nd} {}]
set tr_41 [add_sch_object NODE {an000029} {draw_and2} [list $tr_39 $tr_40]]
set tr_42 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_43 [add_sch_object NODE {an000032} {draw_predicate is_MyFSM__read_gp 1} [list $tr_42]]
set tr_44 [add_sch_object LEAF {an000011} {draw_repeat_nd} {}]
set tr_45 [add_sch_object NODE {an000031} {draw_and2} [list $tr_43 $tr_44]]
set tr_46 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_47 [add_sch_object NODE {an000034} {draw_predicate is_MyFSM__comb 1} [list $tr_46]]
set tr_48 [add_sch_object LEAF {an000004} {draw_repeat_nd} {}]
set tr_49 [add_sch_object NODE {an000033} {draw_and2} [list $tr_47 $tr_48]]
set tr_50 [add_sch_object NODE {an000028} {draw_explicit_or_n 3} [list $tr_41 $tr_45 $tr_49]]
set tr_51 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_52 [add_sch_object NODE {an000036} {draw_predicate is_MyFSM__comb 1} [list $tr_51]]
set tr_53 [add_sch_object LEAF {an000004} {draw_repeat_nd} {}]
set tr_54 [add_sch_object NODE {an000035} {draw_and2} [list $tr_52 $tr_53]]
set tr_55 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_56 [add_sch_object NODE {an000038} {draw_predicate is_MyFSM__read_gp 1} [list $tr_55]]
set tr_57 [add_sch_object LEAF {an000011} {draw_repeat_nd} {}]
set tr_58 [add_sch_object NODE {an000037} {draw_and2} [list $tr_56 $tr_57]]
set tr_59 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_60 [add_sch_object NODE {an000040} {draw_predicate is_MyFSM__pop 1} [list $tr_59]]
set tr_61 [add_sch_object LEAF {an000014} {draw_repeat_nd} {}]
set tr_62 [add_sch_object NODE {an000039} {draw_and2} [list $tr_60 $tr_61]]
set tr_63 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_64 [add_sch_object NODE {an000042} {draw_predicate is_MyFSM__pop 1} [list $tr_63]]
set tr_65 [add_sch_object LEAF {an000014} {draw_repeat_nd} {}]
set tr_66 [add_sch_object NODE {an000041} {draw_and2} [list $tr_64 $tr_65]]
set tr_67 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_68 [add_sch_object NODE {an000044} {draw_predicate is_MyFSM__prim 1} [list $tr_67]]
set tr_69 [add_sch_object LEAF {an000012} {draw_repeat_nd} {}]
set tr_70 [add_sch_object NODE {an000043} {draw_and2} [list $tr_68 $tr_69]]
set tr_71 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_72 [add_sch_object NODE {an000045} {draw_predicate is_MyFSM__alloc 1} [list $tr_71]]
set tr_73 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_74 [add_sch_object NODE {an000046} {draw_predicate is_MyFSM__read_gp 1} [list $tr_73]]
set tr_75 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_76 [add_sch_object NODE {an000048} {draw_predicate is_MyFSM__wba 1} [list $tr_75]]
set tr_77 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_78 [add_sch_object NODE {an000049} {draw_predicate is_MyFSM__wbr 1} [list $tr_77]]
set tr_79 [add_sch_object NODE {an000047} {draw_or2} [list $tr_76 $tr_78]]
set tr_80 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_81 [add_sch_object NODE {an000050} {draw_predicate is_MyFSM__prim 1} [list $tr_80]]
set tr_82 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_83 [add_sch_object NODE {an000051} {draw_predicate is_MyFSM__wba 1} [list $tr_82]]
set tr_84 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_85 [add_sch_object NODE {an000052} {draw_predicate is_MyFSM__wba 1} [list $tr_84]]
set tr_86 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_87 [add_sch_object NODE {an000054} {draw_predicate is_MyFSM__read_gp 1} [list $tr_86]]
set tr_88 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_89 [add_sch_object NODE {an000055} {draw_predicate is_MyFSM__wbr 1} [list $tr_88]]
set tr_90 [add_sch_object NODE {an000053} {draw_or2} [list $tr_87 $tr_89]]
set tr_91 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_92 [add_sch_object NODE {an000056} {draw_predicate is_MyFSM__wbr 1} [list $tr_91]]
set tr_93 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_94 [add_sch_object NODE {an000058} {draw_predicate is_MyFSM__read_gp 1} [list $tr_93]]
set tr_95 [add_sch_object LEAF {an000011} {draw_repeat_nd} {}]
set tr_96 [add_sch_object NODE {an000057} {draw_and2} [list $tr_94 $tr_95]]
set tr_97 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_98 [add_sch_object NODE {an000059} {draw_predicate is_MyFSM__pop 1} [list $tr_97]]
set tr_99 [add_sch_object LEAF {an000001} {draw_repeat_nd} {}]
set tr_100 [add_sch_object NODE {an000060} {draw_predicate is_MyFSM__push_node 1} [list $tr_99]]
set tr_101 [add_sch_object NODE {DummyOut} {draw_output { {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {}}} [list $tr_16 $tr_20 $tr_29 $tr_33 $tr_37 $tr_50 $tr_54 $tr_58 $tr_62 $tr_66 $tr_70 $tr_72 $tr_74 $tr_79 $tr_81 $tr_83 $tr_85 $tr_90 $tr_92 $tr_96 $tr_98 $tr_100]]

draw_network $c $tr_101
