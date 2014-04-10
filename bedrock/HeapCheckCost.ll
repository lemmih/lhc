; ModuleID = 'hello'
 
@.str = private constant [14 x i8] c"Hello, World!\00"
 
declare i32 @puts(i8* nocapture) nounwind
declare i64* @gc_mark(i64 * nocapture) nounwind

declare fastcc void @do_stuff(i64* , i64* , i64* ) nounwind noreturn
 
define i32 @main() {
  %1 = call i32 @puts(i8* getelementptr inbounds ([14 x i8]* @.str, i32 0, i32 0))
  ret i32 0
}
!0 = metadata !{
		metadata !"branch_weights",
		i32 0,
		i32 100000
	}
define void @loop(i64* %hp, i64* %hpLim) {
	call fastcc void @before_heap_check(i64* %hp, i64* %hpLim, i64* %hp)
	tail call void @loop(i64* %hp, i64* %hpLim)
	ret void
}
define private fastcc void @before_heap_check(i64* %hp, i64* %hpLim, i64* %arg) {
	%hpNew = getelementptr i64* %hp, i64 2
	%cond = icmp ult i64* %hpNew, %hpLim
	
	br i1 %cond, label %HaveMem, label %OOM
HaveMem:
	tail call fastcc void @after_heap_check(i64* %hpNew, i64* %hpLim, i64* %arg)
	ret void
OOM:
	tail call fastcc void @indirection(i64* %hp, i64* %hpLim, i64* %arg)
	;%argNew = call coldcc i64* @gc_mark(i64* %arg)
	;tail call fastcc void @after_heap_check(i64* %hpNew, i64* %hpLim, i64* %argNew)
	;tail call fastcc void @after_heap_check(i64* %hp, i64* %hpLim, i64* %arg)
	ret void
}

define private fastcc void @indirection(i64* %hp, i64* %hpLim, i64* %arg) {
	%argNew = call coldcc i64* @gc_mark(i64* %arg)
	tail call fastcc void @after_heap_check(i64* %hp, i64* %hpLim, i64* %argNew)
	ret void
}


define private fastcc void @after_heap_check(i64* %hp, i64* %hpLim, i64* %arg) {
	tail call fastcc void @do_stuff(i64* %hp, i64* %hpLim, i64* %arg)
	ret void
}

