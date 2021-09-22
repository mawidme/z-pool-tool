let migrations =
  let open Database.Migration in
  [ tenant () ]
;;

let commands = [ Command.Tenant.create_tenant; Command.Tenant.seed_tenants ]

let services =
  [ Sihl.Database.register ()
  ; Service.User.register ()
  ; Service.Token.register ()
  ; Service.Migration.(register migrations)
  ; Sihl.Web.Http.register ~middlewares:Routes.global_middlewares Routes.router
  ]
;;

(* This is the entry point of your Sihl app *)
let () =
  Sihl.App.(
    empty
    |> with_services services
    |> before_start (fun () -> Printexc.record_backtrace true |> Lwt.return)
    |> run ~commands)
;;
