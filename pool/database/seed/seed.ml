module Root = struct
  let create () =
    let%lwt () = Seed_tenant.create () in
    Lwt.return_unit
  ;;
end

module Tenant = struct
  let create db_pools () =
    Lwt_list.iter_s
      (fun pool ->
        let%lwt () = Seed_settings.create pool () in
        let%lwt () = Seed_user.admins pool () in
        let%lwt () = Seed_user.participants pool () in
        let%lwt () = Seed_i18n.i18n pool () in
        Lwt.return_unit)
      db_pools
  ;;
end