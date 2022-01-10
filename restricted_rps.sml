(* Players have three resources:
1. Cards; rock, paper or scissors
2. Stars: every player starts off with three, and must retain three or more till
   he finishes all his cards to win 
3. Money: can buy/sell cards for money. Won't include this in the first iteration,
   but will have agents later who trade/sell money*)

datatype card = Rock | Paper | Scissor;
datatype result = Win | Loss | Draw;

exception OutOfStars;
exception OutOfCards;

fun intToCard 1 = Rock | intToCard 2 = Paper | intToCard 3 = Scissor;
fun decrCards (0,0,0) idx = raise OutOfCards 
  | decrCards (r,p,s) idx = 
	if idx = 1 andalso r > 0 then (r-1,p,s)
	else if idx = 2 andalso p > 0 then (r,p-1,s)
	else if idx = 3 andalso s > 0 then (r,p,s-1)
	else raise OutOfCards;

fun check (Rock, Rock) = Draw | check (Paper, Paper) = Draw | check (Scissor, Scissor) = Draw 
  | check (Paper, Rock) = Win 
  | check (Scissor, Paper) = Win
  | check (Rock, Scissor) = Win
  | check (b,c) = Loss;

fun shuffle rng _ 0 = []
  | shuffle rng l len = 
let
	val rnum = Random.randRange (0,len-1) rng
	val relem = List.nth (l, rnum)
	val rlist = List.take (l, rnum) @ List.drop (l, rnum+1)
in
	[relem] @ (shuffle rng rlist (len-1))
end

fun randomStrategy (0,0,0) rng = raise OutOfCards
  | randomStrategy cards rng = 
let
	val a::b::c::[] = shuffle rng (List.tabulate (3, fn a => (a+1))) 3
in
	(intToCard a, decrCards cards a)
	handle OutOfCards => (intToCard b, decrCards cards b)
	handle OutOfCards => (intToCard c, decrCards cards c)
end

(* requires WAY more nuance: (a,b,c), (a,a,b) and (b,b,b) cases
fun balancedStrategy (0,0,0) rng = raise OutOfCards
  | balancedStrategy (r,p,s) rng = 
  	if r > p andalso r > s then (Rock, (r-1,p,s))
	else if p > s then (Paper, (r,p-1,s))
	else if r=p andalso r=s andalso s=p then randomStrategy (r,p,s) (Scissor, (r,p,s-1))
	else randomStrategy (r,p,s) rng
*)

fun initPlayers 0 c = [] 
  | initPlayers n c = {strat=randomStrategy, cards=(c,c,c), stars=3} :: initPlayers (n-1) c;


fun play rng ({strat=st1, cards=c1, stars=s1},{strat=st2, cards=c2, stars=s2}) =
let
	val (t1,c1') = st1 c1 rng
	val (t2,c2') = st2 c2 rng

	val result = check (t1,t2)
in
	if result = Win then ({strat=st1,cards=c1',stars=(s1+1)}, {strat=st2,cards=c2',stars=(s2-1)})
	else if result = Loss then ({strat=st1,cards=c1',stars=(s1-1)}, {strat=st2,cards=c2',stars=(s2+1)})
	else ({strat=st1,cards=c1',stars=s1}, {strat=st2,cards=c2',stars=s2})
end

(* 
 * Those who lost all stars are immediately pushed out of the floor.
 * Those who won more than three stars 
 *)

fun checkWin {strat=st, cards=(r,p,x), stars=s} = (s >= 3) andalso ((r+p+x) = 0);
fun checkLose {strat=st, cards=(r,p,x), stars=s} = (s = 0)
fun checkStay r = not (checkWin r orelse checkLose r);

fun simIter (winners,floor,losers) rng =
let
	val randomFloor = shuffle rng floor (length floor)
	val matchups = ListPair.zip (List.take (floor, length(floor) div 2), List.drop (floor, length(floor) div 2))
	val results = List.map (play rng) matchups 
	val (l,r) = ListPair.unzip results
	val newFloor = l @ r
	val newWinners = winners @ (List.filter checkWin newFloor)
	val newLosers = losers @ (List.filter checkLose newFloor)
	val nextFloor = List.filter checkStay newFloor
in
	(newWinners, nextFloor, newLosers)
end

fun rounds (w,f,l) rno cards rng = 
let
	val _ = print ("Round " ^ Int.toString(rno) ^ ": " ^ ((Int.toString(length w)) ^ " people won, " ^ Int.toString(length f) ^ " people on the floor and " ^ (Int.toString(length l)) ^ " people lost.\n"))
in
	if rno = (cards*3) then (w,f@l) else (rounds (simIter (w,f,l) rng) (rno+1) cards rng)
end

fun simulate n c =
let
	val winners = []
	val losers = []
	val seed = Real.floor (Time.toReal(Time.now()) - 1.48e9)
	val rng = Random.rand (seed,seed+5)
	val floor = initPlayers n c
	val (w,l) = rounds (winners,floor,losers) 0 c rng
in
	print ((Int.toString(length w)) ^ " people won and " ^ (Int.toString(length l)) ^ " people lost.\n")
end

