let unimpl () = failwith "Unimpl"
let print x = ()


module U = Unix

module SQL = struct
  let select = unimpl ()
  let select1 = unimpl ()
  let insert = unimpl ()
  let delete = unimpl ()
  let update = unimpl ()
end

module S = struct
  include List
  let size = length
  let foreach = iter
  let max f l = unimpl ()
  let min f l = unimpl ()
  let sum f l = unimpl ()
end

type id

type table_name = 
  | District
  | Warehouse
  | Customer
  | History
  | Order
  | New_order
  | Order_line
  | Stock
  | Item

type warehouse = {w_id: id; mutable w_ytd: int}
type district = {d_id: id; d_w_id: id; 
                 mutable d_ytd: int; 
                 mutable d_next_o_id: int}
type customer = {c_id: id; c_d_id: id; c_w_id: id; 
                 mutable c_bal: int; 
                 mutable c_ytd_payment:int; 
                 mutable c_payment_cnt:int; 
                 mutable c_delivery_cnt:int;}
type order = {o_id:int; o_w_id: id; o_d_id: id; 
              o_c_id: id; o_ol_cnt: int; 
              mutable o_carrier_id: unit option}
type new_order= {no_o_id: int; no_d_id: id; no_w_id: id}
type order_line = {ol_o_id: int; ol_d_id: id; 
                   ol_w_id: id; ol_num: int; ol_amt: int; 
                   ol_i_id: id; ol_supply_w_id: id;
                   ol_qty: int; mutable ol_delivery_d: Unix.tm option}
type hist = {h_c_id: id; h_c_d_id: id; h_c_w_id: id; 
             h_d_id: id; h_w_id: id; h_amt: int}
type item = {i_id: id; i_name: string; i_price: int}
type stock = {s_i_id: id; s_w_id: id; mutable s_qty: int; 
              mutable s_ytd: int; mutable s_order_cnt: int}

type item_req = {ol_num: int (* seq no *); ol_i_id: id; 
                 ol_supply_w_id: id; ol_qty: int}
(*
 * New Order transaction. 
 *)
let new_order_txn w_id d_w_id d_id c_w_id c_d_id c_id
      (ireqs: item_req list) =
  let dist = SQL.select1 [District] 
               (fun d -> d.d_id = d_id && d.d_w_id = w_id) in
  let o_id = dist.d_next_o_id in
  let d_next_o_id' = o_id + 1 in
  let _ = SQL.update District 
            (fun d -> d.d_next_o_id <- d_next_o_id')
            (fun d -> d.d_id = d_id && d.d_w_id = w_id) in
  let ord = {o_id=o_id; o_w_id=w_id; o_d_id=d_id; 
             o_c_id=c_id; o_ol_cnt=S.size ireqs; 
             o_carrier_id = None} in
  let new_ord = {no_o_id=o_id; no_w_id=w_id; no_d_id=d_id} in
    begin
      SQL.insert Order ord;
      SQL.insert New_order new_ord;
      S.foreach
        (fun ireq -> 
          let stk = SQL.select1 [Stock] 
                      (fun s -> s.s_i_id = ireq.ol_i_id &&
                                s.s_w_id = ireq.ol_supply_w_id) in
          let item = SQL.select1 [Item]
                       (fun item -> item.i_id = ireq.ol_i_id) in
          let ol = {ol_o_id=o_id; ol_d_id=d_id; ol_w_id=w_id; 
                    ol_num=ireq.ol_num; ol_i_id=ireq.ol_i_id; 
                    ol_supply_w_id=ireq.ol_supply_w_id; 
                    ol_amt=item.i_price * ireq.ol_qty;
                    ol_qty=ireq.ol_qty; ol_delivery_d=None} in
          let s_qty = if stk.s_qty >= ireq.ol_qty + 10 
                      then stk.s_qty - ireq.ol_qty
                      else stk.s_qty - ireq.ol_qty + 91 in
            begin
             SQL.update Stock
                (fun s -> 
                   begin 
                     s.s_qty <- s_qty;
                     s.s_ytd <- stk.s_ytd + ireq.ol_qty; 
                     s.s_order_cnt <- stk.s_order_cnt + 1
                   end)
                (fun s -> s.s_i_id = ireq.ol_i_id &&
                          s.s_w_id = ireq.ol_supply_w_id);
              SQL.insert Order_line ol
            end) 
        ireqs
    end

(*
 * Payment transaction.
 * Pre: w_id == d_w_id 
 *)
let payment_txn w_id d_id d_w_id c_w_id c_d_id c_id h_amt = 
  let w = SQL.select1 [Warehouse] (fun wh -> wh.w_id = w_id) in
  let d = SQL.select1 [District] 
            (fun dist -> dist.d_w_id = d_w_id && 
                         dist.d_id = d_id) in
  let c = SQL.select1 [Customer] 
            (fun cs -> cs.c_w_id = c_w_id && 
                       cs.c_d_id = c_d_id && 
                       cs.c_id = c_id) in
  let h_item = {h_c_id = c_id; h_c_d_id = c_d_id; h_c_w_id = c_w_id; 
                h_d_id = d_id; h_w_id = w_id; h_amt = h_amt} in
    begin
      SQL.update Warehouse 
        (fun wh -> wh.w_ytd <- w.w_ytd + h_amt)
        (fun wh -> wh.w_id = w_id);
      SQL.update District
        (fun dist -> dist.d_ytd <- d.d_ytd + h_amt)
        (fun dist -> dist.d_w_id = d_w_id && 
                     dist.d_id = d_id);
      SQL.update Customer
        (fun cs -> 
           begin
             cs.c_bal <- c.c_bal - h_amt;
             cs.c_ytd_payment <- c.c_ytd_payment + h_amt;
             cs.c_payment_cnt <- c.c_payment_cnt + 1
           end)
        (fun cs -> cs.c_w_id = c_w_id && 
                   cs.c_d_id = c_d_id && 
                   cs.c_id = c_id);
      SQL.insert History h_item
    end

(*
 * Order Status (read-only) transaction.
 *)
let order_status_txn c_w_id c_d_id c_id = 
  let c = SQL.select1 [Customer]
            (fun cs -> cs.c_w_id = c_w_id && 
                      cs.c_d_id = c_d_id && 
                      cs.c_id = c_id) in
  let ords = SQL.select [Order] 
               (fun o -> o.o_w_id = c_w_id && 
                         o.o_d_id = c_d_id && 
                         o.o_c_id = c_id) in
  (* sort ords in decreasing order *)
  let o = S.max (fun o -> o.o_id) ords in
  let ols = SQL.select [Order_line] 
              (fun ol -> ol.ol_w_id = o.o_w_id && 
                         ol.ol_d_id = o.o_d_id && 
                         ol.ol_o_id = o.o_id) in
    (c.c_bal,ols)

(*
 * Delivery transaction.
 *)
let delivery_txn w_id =
  let dists = SQL.select [District] (fun d -> d.d_w_id = w_id) in
    S.foreach (fun d ->
       let nords = SQL.select [New_order] 
                     (fun no -> no.no_w_id = w_id && 
                                no.no_d_id = d.d_id) in
       let no = S.min (fun no -> no.no_o_id) nords in
       (* delete the no entry *)
       let _ = SQL.delete [New_order] 
                     (fun n -> n.no_o_id = no.no_o_id && 
                               n.no_w_id = w_id && 
                               n.no_d_id = d.d_id) in
       let o = SQL.select1 [Order]  
                 (fun o -> o.o_w_id = w_id && 
                           o.o_d_id = d.d_id && 
                           o.o_id = no.no_o_id) in 
       let _ = SQL.update Order
                 (fun o -> o.o_carrier_id <- Some ())
                 (fun o -> o.o_w_id = w_id && 
                           o.o_d_id = d.d_id && 
                           o.o_id = no.no_o_id) in
       let _ = SQL.update Order_line
                 (fun ol -> 
                    ol.ol_delivery_d <- Some (U.gmtime @@ U.time ()))
                 (fun ol -> ol.ol_w_id = o.o_w_id && 
                            ol.ol_d_id = o.o_d_id && 
                            ol.ol_o_id = o.o_id) in
       let ols = SQL.select [Order_line] 
                   (fun ol -> ol.ol_w_id = o.o_w_id && 
                              ol.ol_d_id = o.o_d_id && 
                              ol.ol_o_id = o.o_id) in
       let amt = S.sum (fun ol -> ol.ol_amt) ols in
       (*
        * Following can be replaced by SQL's atomic UPDATE.
        *)
       let c = SQL.select1 [Customer]
                 (fun c -> c.c_w_id = w_id && 
                           c.c_d_id = d.d_id && 
                           c.c_id = o.o_c_id) in
       let _ = SQL.update Customer 
                (fun c' -> 
                   begin
                      c'.c_bal <- c.c_bal + amt;
                      c'.c_delivery_cnt <- c.c_delivery_cnt + 1;
                   end)
                (fun c -> c.c_w_id = w_id && 
                          c.c_d_id = d.d_id && 
                          c.c_id = o.o_c_id) in
        ()) dists

(*
 * Stock-level (read-only) transaction.
 *)
let stock_level_txn w_id d_w_id d_id thres = 
  let dist = SQL.select1 [District] 
               (fun dist -> dist.d_w_id = d_w_id && 
                            dist.d_id = d_id) in
  let next_o_id = dist.d_next_o_id in
  let ols = SQL.select [Order_line] 
              (fun ol -> ol.ol_w_id = w_id && 
                         ol.ol_d_id = d_id && 
                         ol.ol_o_id < next_o_id && 
                         ol.ol_o_id >= next_o_id - 20) in
    S.foreach 
      (fun ol -> 
        let stk = SQL.select1 [Stock] 
                    (fun s -> s.s_i_id = ol.ol_i_id && 
                              s.s_w_id = w_id) in
          print (ol,stk)) ols

(* Invariants *)
let inv1 = fun (w:warehouse) -> 
  let dists = SQL.select [District] 
                (fun d -> d.d_w_id = w.w_id) in
  let sum_d_ytd = S.sum (fun d -> d.d_ytd) dists in 
    w.w_ytd = sum_d_ytd

let inv2 = fun (w:warehouse) ->
  let dists = SQL.select [District] 
                (fun d -> d.d_w_id = w.w_id) in
    S.for_all 
      (fun d -> 
         let ords_from_dist = SQL.select [Order] 
                (fun o -> o.o_w_id = w.w_id &&
                          o.o_d_id = d.d_id) in
         let nords_from_dist = SQL.select [New_order]
                (fun no -> no.no_w_id = w.w_id &&
                           no.no_d_id = d.d_id) in
         let max_o_id_from_dist = 
           S.max (fun o -> o.o_id) ords_from_dist in
         let max_no_o_id_from_dist = 
           S.max (fun no -> no.no_o_id) nords_from_dist in
           max_o_id_from_dist = max_no_o_id_from_dist &&
           max_o_id_from_dist = d.d_next_o_id - 1
      ) dists

let inv3 = fun (w:warehouse) ->
  let dists = SQL.select [District] 
                (fun d -> d.d_w_id = w.w_id) in
    S.for_all 
      (fun d -> 
         let nords_from_dist = SQL.select [New_order]
                (fun no -> no.no_w_id = w.w_id &&
                           no.no_d_id = d.d_id) in
         let max_no_o_id_from_dist = 
           S.max (fun no -> no.no_o_id) nords_from_dist in
         let min_no_o_id_from_dist = 
           S.min (fun no -> no.no_o_id) nords_from_dist in
         let n_nords_for_dist = S.size nords_from_dist in
           max_no_o_id_from_dist - min_no_o_id_from_dist + 1 =
             n_nords_for_dist
      ) dists

let inv4 =  fun (w:warehouse) ->
  let dists = SQL.select [District] 
                (fun d -> d.d_w_id = w.w_id) in
    S.for_all 
      (fun d -> 
         let ords_from_dist = SQL.select [Order] 
                (fun o -> o.o_w_id = w.w_id &&
                          o.o_d_id = d.d_id) in
         let sum_o_ol_cnt = S.sum (fun o -> o.o_ol_cnt)
                              ords_from_dist in
         let ols_from_dist = SQL.select [Order_line]
                (fun ol -> ol.ol_w_id = w.w_id &&
                           ol.ol_d_id = d.d_id) in
         let n_ols_from_dist = S.size ols_from_dist in
           sum_o_ol_cnt = n_ols_from_dist
      ) dists

let inv5 = fun (o:order) -> 
  let nos = SQL.select [New_order]
               (fun no -> no.no_w_id = o.o_w_id &&
                          no.no_d_id = o.o_d_id && 
                          no.no_o_id = o.o_id) in
    match (o.o_carrier_id,nos) with
      | (None, [no]) -> true
      | (Some (), []) -> true
      | _ -> false

let inv6 = fun (o:order) -> 
  let n_ols_for_o = S.size @@ SQL.select [Order_line]
               (fun ol -> ol.ol_w_id = o.o_w_id &&
                          ol.ol_d_id = o.o_d_id && 
                          ol.ol_o_id = o.o_id) in
    o.o_ol_cnt = n_ols_for_o

let inv7 = fun (ol:order_line) ->
  let o_of_ol = SQL.select1 [Order]
                  (fun o -> o.o_w_id = ol.ol_w_id && 
                            o.o_d_id = ol.ol_d_id && 
                            o.o_id = ol.ol_o_id) in
    match (ol.ol_delivery_d, o_of_ol.o_carrier_id) with
      | (None,None) | (Some _, Some _) -> true
      | _ -> false

let inv8 = fun (w:warehouse) ->
  let hist = SQL.select [History] 
                 (fun h -> h.h_w_id = w.w_id) in
  let sum_h_amt = S.sum (fun h -> h.h_amt) hist in
    w.w_ytd = sum_h_amt

let inv9 = fun (d:district) ->
  let hist = SQL.select [History] 
               (fun h -> h.h_d_id = d.d_id) in
  let sum_h_amt = S.sum (fun h -> h.h_amt) hist in
    d.d_ytd = sum_h_amt

let inv10 = fun (c:customer) ->
  let ols_of_c = SQL.select [Order_line,Order]
                   (fun ol -> fun o -> 
                      ol.ol_w_id = c.c_w_id &&
                      ol.ol_d_id = c.c_d_id &&
                      ol.ol_o_id = o.o_id &&
                      o.o_c_id = c.c_id) in
  let hist_of_c = SQL.select [History] 
                    (fun h -> h.h_c_w_id = c.c_w_id && 
                              h.h_c_d_id = c.c_d_id && 
                              h.h_c_id = c.c_id) in
  let sum_ol_amt = S.sum (fun ol -> ol.ol_amt) ols_of_c in
  let sum_h_amt = S.sum (fun h -> h.h_amt) hist_of_c in
    c.c_bal = sum_ol_amt - sum_h_amt

let inv11 = fun () -> true

let inv12 = fun (c:customer) ->
  let delivered_ols_of_c = 
          SQL.select [Order_line,Order]
                   (fun ol -> fun o -> 
                      ol.ol_w_id = c.c_w_id &&
                      ol.ol_d_id = c.c_d_id &&
                      ol.ol_o_id = o.o_id &&
                      o.o_c_id = c.c_id) in
  let sum_delivered_ol_amt = S.sum (fun ol -> ol.ol_amt) 
                               delivered_ols_of_c in
    c.c_bal + c.c_ytd_payment = sum_delivered_ol_amt
