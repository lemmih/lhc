
declare void @touch(i64) nounwind
declare i64 @make() nounwind


define void @step1() {
  %v1 = add i64 0, 0
  tail call fastcc void @step2(i64 %v1)
  ret void
}
define fastcc void @step2(i64 %v1) {
  %v2 = add i64 %v1, 1
  tail call fastcc void @step3(i64 %v2, i64 %v1)
  ret void
}
define fastcc void @step3(i64 %v2, i64 %v1) {
  %v3 = add i64 %v2, 1
  tail call fastcc void @step4(i64 %v3, i64 %v2, i64 %v1)
  ret void
}
define fastcc void @step4(i64 %v3, i64 %v2, i64 %v1) {
  %v4 = add i64 %v3, 1
  tail call fastcc void @step5(i64 %v4, i64 %v3, i64 %v2, i64 %v1)
  ret void
}
define fastcc void @step5(i64 %v4, i64 %v3, i64 %v2, i64 %v1) {
  %v5 = add i64 %v4, 1
  tail call fastcc void @step6(i64 %v5, i64 %v4, i64 %v3, i64 %v2, i64 %v1)
  ret void
}
define fastcc void @step6(i64 %v5, i64 %v4, i64 %v3, i64 %v2, i64 %v1) {
  %v6 = add i64 %v5, 1
  %v7 = call i64 @make()
  %v8 = call i64 @make()
  %v9 = call i64 @make()
  %v10 = call i64 @make()
  %v11 = call i64 @make()
  %v12 = call i64 @make()
  %v13 = call i64 @make()
  %v14 = call i64 @make()
  %v15 = call i64 @make()
  %v16 = call i64 @make()
  %v17 = call i64 @make()
  %v18 = call i64 @make()
  %v19 = call i64 @make()
  %v20 = call i64 @make()
  tail call fastcc void @step7(
    i64 %v7, i64 %v8, i64 %v9, i64 %v10, i64 %v11, i64 %v12,
    i64 %v13, i64 %v14, i64 %v15, i64 %v16, i64 %v17, i64 %v18,
    i64 %v19, i64 %v20,
    i64 %v6, i64 %v5, i64 %v4, i64 %v3, i64 %v2, i64 %v1)
  ret void
}


define fastcc void @step7(
  i64 %v7, i64 %v8, i64 %v9, i64 %v10,
  i64 %v11, i64 %v12, i64 %v13, i64 %v14, i64 %v15,
  i64 %v16, i64 %v17, i64 %v18, i64 %v19, i64 %v20,
  i64 %v1, i64 %v2, i64 %v3, i64 %v4, i64 %v5, i64 %v6) {

  ;call void @touch(i64 %v1)
  ;call void @touch(i64 %v2)
  ;call void @touch(i64 %v20)
  %next = add i64 %v7, 1
  %v13_ = add i64 %v13, 1
  %v14_ = add i64 %v14, 1
  %v20_ = add i64 %v20, 1
  tail call fastcc void @step7(i64 %v7, i64 %v8, i64 %v9, i64 %v10, i64 %v11, i64 %v12, i64 %v13, i64 %v14, i64 %v15, i64 %v16, i64 %v17, i64 %v18, i64 %v19, i64 %v20_,
  i64 %v1, i64 %v2, i64 %v3, i64 %v4, i64 %v5, i64 %v6)
  ret void
}
