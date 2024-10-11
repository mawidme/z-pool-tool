let cancelable session m =
  Http_utils_session.assignments_cancelable session
  && Assignment.is_cancellable m |> CCResult.is_ok
;;

let deletable = CCFun.(Assignment.is_deletable %> CCResult.is_ok)
