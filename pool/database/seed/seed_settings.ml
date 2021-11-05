let sql =
  {|
  INSERT INTO `pool_system_settings` (`uuid`, `settings_key`, `value`) VALUES
  (UNHEX('7810B669ECA241758EAD0A2217378B58'), '["languages"]', '[["EN"],["DE"]]'),
  (UNHEX('45CB6D1659D94CFBA628C66FA152EB0B'), '["email_suffixes"]', '["econ.uzh.ch","uzh.ch"]'),
  (UNHEX('9CA82A68E8BE471F82CE6B15E86CFED6'), '["contact_email"]', '"pool@econ.uzh.ch"'),
  (UNHEX('04B3166067494F35B03B437DB42E2CE9'), '["inactive_user_disable_after"]', "5"),
  (UNHEX('54B9E26C24624F12AF85131C424D1843'), '["inactive_user_warning"]', "7");
  |}
  |> Caqti_request.exec Caqti_type.unit
;;

let create pool =
  Utils.Database.exec (Pool_common.Database.Label.value pool) sql
;;
