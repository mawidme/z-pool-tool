open Entity
module Common = Pool_common
module Id = Common.Id
module RepoId = Common.Repo.Id

module Title = struct
  include Title

  let t = Caqti_type.string
end

module Description = struct
  include Description

  let t = Caqti_type.string
end

let t =
  let encode (m : t) =
    Ok
      ( Id.value m.id
      , ( Title.value m.title
        , ( Description.value m.description
          , (m.filter, (m.created_at, m.updated_at)) ) ) )
  in
  let decode (id, (title, (description, (filter, (created_at, updated_at))))) =
    let open CCResult in
    map_err (fun _ ->
        Common.(
          Utils.error_to_string
            Common.Language.En
            (Message.Decode Message.Field.I18n)))
    @@ let* title = Title.create title in
       let* description = Description.create description in
       Ok
         { id = Id.of_string id
         ; title
         ; description
         ; filter
         ; created_at
         ; updated_at
         }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         RepoId.t
         (tup2
            Title.t
            (tup2
               Description.t
               (tup2
                  string
                  (tup2 Common.Repo.CreatedAt.t Common.Repo.UpdatedAt.t))))))
;;

module Write = struct
  let t =
    let encode (m : t) =
      Ok
        ( Id.value m.id
        , (Title.value m.title, (Description.value m.description, m.filter)) )
    in
    let decode _ = failwith "Write only model" in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2 RepoId.t (tup2 Title.t (tup2 Description.t string))))
  ;;
end
