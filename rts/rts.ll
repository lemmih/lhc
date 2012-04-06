; LHC RTS:

%unit = type i32

; We return function results in this global array.
%returnArrayT = type [20 x %unit]
@rtsReturnArray = global %returnArrayT zeroinitializer

define %unit @getReturnValue(i32 %idx) {
  %retPtr = getelementptr %returnArrayT* @rtsReturnArray, i32 0, i32 %idx
  %val = load %unit* %retPtr
  store %unit 0, %unit* %retPtr
  ret %unit %val
}

define %unit* @getReturnValuePtr(i32 %idx) {
  %retPtr = getelementptr %returnArrayT* @rtsReturnArray, i32 0, i32 %idx
  ret %unit* %retPtr
}

define void @setReturnValue(i32 %idx, %unit %val) {
  %retPtr = call %unit* @getReturnValuePtr(i32 %idx)
  store %unit %val, %unit* %retPtr
  ret void
}

; End of LHC RTS.
