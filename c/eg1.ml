open Utils

type table_name = 
  | User_account

type user_account = {id: id; name: string; 
                     mutable cbal: int; mutable sbal: int}
(*
 * Withdraw transaction
 * G = {(σ,σ') | I(σ) ∧ I(σ') ∧ 
 *               ∃ρ.∃x. ρ∈dom(σ) ∧ σ'=σ[ρ → σ(ρ) with cbal=x]}
 *)
let withdraw_txn u_id amt = 
  let user = SQL.select1 [User_account] 
               (fun u -> u.id = u_id) in
    if user.cbal >= amt 
    then
      SQL.update User_account
        (fun u -> u.cbal <- user.cbal - amt)
        (fun u -> u.id = u_id)
    else
      ()
(*
 * Deposit transaction
 * G = {(σ,σ') | I(σ) ∧ I(σ') ∧ 
 *               ∃ρ.∃x. ρ∈dom(σ) ∧ σ'=σ[ρ → σ(ρ) with cbal=x]}
 *)
let deposit_txn u_id amt = 
  let user = SQL.select1 [User_account] 
               (fun u -> u.id = u_id) in
      SQL.update User_account
        (fun u -> u.cbal <- user.cbal + amt)
        (fun u -> u.id = u_id)
(*
 * Save transaction
 * G = {(σ,σ') | I(σ) ∧ I(σ') ∧ 
 *               ∃ρ.∃x.∃y. ρ∈dom(σ) ∧ σ'=σ[ρ → σ(ρ) 
 *                                      with cbal=x ∧ sbal=y]}
 *)
let save_txn u_id amt = 
  let user = SQL.select1 [User_account] 
               (fun u -> u.id = u_id) in
    if user.cbal >= amt 
    then
      SQL.update User_account
        (fun u -> 
           begin
             u.cbal <- user.cbal - amt;
             u.sbal <- user.sbal + amt;
           end)
        (fun u -> u.id = u_id)
    else
      ()

(*
 * Invariants
 *)
let inv1 () = 
  forall (u:User_account). u.cbal >=0 && u.sbal >=0

