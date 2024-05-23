(* Définition des types  couleur et code *)
type couleur = Cyan |  Bleu | Blanc | Vert | Magenta | Jaune | Noir | Rouge
type code = couleur list

(* Fonction pour générer un code aléatoire  cad une combinaison *)
let generer_code n couleurs =
  let rec aux n liste_combinaisonAgenerer =
    if n = 0 then liste_combinaisonAgenerer
    else
      (* cette expression couleur_aleatoire prend en argument la liste des couleur et la position de chaque couleur dans la liste *)
      let couleur_aleatoire = List.nth couleurs (Random.int (List.length couleurs)) in
      aux (n-1) (couleur_aleatoire :: liste_combinaisonAgenerer) (* cette expression permet d'ajouter  le couleur  prise dans la liste appelée 
                                                                 'liste_combinaisonAgenerer' au debut de la liste   *)
  in aux n []

(* Fonction pour comparer  les deux codes le code entré par le joueur et le code défini par la machine et 
la fonction va retourner le nombre de couleurs bien placées et mal placées*)
let comparer_codes code_secret code_propose =
  let rec comparerCombinaison code_s code_p bien_place mal_place =
    match code_s, code_p with
    | [], _ | _, [] -> (bien_place, mal_place)
    | h1::t1, h2::t2 ->
        if h1 = h2 then comparerCombinaison t1 t2 (bien_place + 1) mal_place
        else if List.mem h2 code_s then comparerCombinaison t1 t2 bien_place (mal_place + 1)
        else comparerCombinaison t1 t2 bien_place mal_place
  in comparerCombinaison code_secret code_propose 0 0

(* Fonction principale pour jouer le jeu *)
let jouer_mastermind () =
  let couleurs = [Rouge; Vert; Jaune; Bleu; Magenta; Blanc; Noir; Cyan] in
  let code_secret = generer_code 4 couleurs in
  let limite_tentatives = 10 in (*  le nombre maximal de tentatives possibles *)
  let rec jouer_tour tentative =
    if tentative > limite_tentatives then
      begin
        print_endline " le nombre de tentatives depassé . Vous avez perdu !";
        exit 0 (* Terminer le programme *)
      end;
    Printf.printf "Tentative %d/%d. Proposez un code : " tentative limite_tentatives;
    let code_propose = read_line () |> String.split_on_char ' ' |> List.map (fun s -> match s with
        | "Rouge" -> Rouge
        | "Vert" -> Vert
        | "Jaune" -> Jaune
        | "Bleu" -> Bleu
        | "Magenta" -> Magenta
        | "Blanc" -> Blanc
        | "Noir" -> Noir
        | "Cyan" -> Cyan
        | _ -> failwith "Couleur non reconnue") in
    let (bien_place, mal_place) = comparer_codes code_secret code_propose in
    if bien_place = List.length code_secret then begin
      print_endline "Bravo, vous avez trouvé le code !";
      exit 0 (* Terminer le programme en cas de victoire *)
    end else begin
      Printf.printf "Bien placés : %d, Mal placés : %d\n" bien_place mal_place;
      
       (* Incrémenter la tentative pour le prochain tour *)
      jouer_tour (tentative + 1) 
    end
  in jouer_tour 1 (* Commencer avec la tentative num 1 *)

(* Lancement du jeu *)
let () = Random.self_init (); jouer_mastermind ()
