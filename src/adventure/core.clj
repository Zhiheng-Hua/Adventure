(ns adventure.core
  (:gen-class))

;; player starts at 0
;; boss starts at 2
(def maze  {0 [10 12 13] 1 [2 5] 	 2 [1 4 6] 	3 [4 7] 	4 [2 3 11]
			5 [1 6 8]	 6 [2 5 7 9] 7 [3 6 11] 8 [5 12 13] 9 [6 10 12]
			10 [0 9 11]  11 [4 7 10] 12 [0 8 9] 13 [0 8]})

(def item-description 
	{"ARMOR" {:desc "This is a firm and reliable armor that can protect you from danger."
			  :name "ARMOR"}
	"SWORD" {:desc "This is a sharp sword prepared for the warrior."
		     :name "SWORD"}
	"fire-resist" {:desc "This is a bottle of fire-resist potion, which can prevent you from the damage of fire."
				   :name "fire-resist"}
	"lightning-resist" {:desc "This is a bottle of lightning-resist potion, which can prevent you from the damage of lightning"
					    :name "lightning-resist"}
	"poison-resist" {:desc "This is a bottle of poison-resist potion, which can prevent you from the damage of poison"
				     :name "poison-resist"}})

(def room-description 
  { 1 {:name "Castle"
       :desc "An old castle with all the doors and windows sealed with wood plates."}
    2 {:name "Dragon's spawn"
       :desc "This is the spawn of Evil Dragon. Dark and gloomy..."}
    3 {:name "Fort"
       :desc "A suspicious fort… You don't want to stay here for long."}
    4 {:name "Tomb"
       :desc "You ran into a mysterious tomb."}
    5 {:name "Volcano"
       :desc "You noticed an unstable volcano. Be aware of any incoming dangers."}
    6 {:name "Twilight Forest"
       :desc "This is the notorious Twilight Forest. You don't want to be careless here."}
    7 {:name "Desert"
       :desc "A desert called Lahara. Don't get heat stroke."}
    8 {:name "Mine"
       :desc "This place seems like a mine during the war time, but there is nothing here anymore..."}
    9 {:name "Cornfield"
       :desc "There is nothing but only cornfields."}
    10 {:name "Stawford"
        :desc "A town named Stawford. It seems it was just newly abandoned."}
    11 {:name "Swamp"
        :desc "Many swamps... Be careful!"}
    12 {:name "Prairie"
        :desc "A vast prairie. Seems you can learn something here."}
    13 {:name "Acton"
        :desc "Here is an abandoned town. According to the map, it was called Acton."}
    0  {:name "Your Spawn"
        :desc "This is your spawn, the home of the brave." }})

(def maze-size (count maze))

(defn rand-unique
    "Pick a random number from 0 to `max-1` that is not in the set `exclude`.  Does not check for errors."
    [max exclude]
    (let [pick (rand-int max)]
        (if (exclude pick) (rand-unique max exclude) pick)))

(defn new-game []
	"Generate the initial state of the game, return the state,
	 contains information of boss, player, and items in the maze by mapping room# to item name"
  	(let [armor (rand-unique maze-size #{0})
		  fire-resist (rand-unique maze-size #{0 armor})
		  lightning-resist (rand-unique maze-size #{0 armor fire-resist})
		  poison-resist (rand-unique maze-size #{0 armor fire-resist lightning-resist})
		  sword (rand-unique maze-size #{0 armor fire-resist lightning-resist poison-resist})]
		{:boss   {:health 20  		:location 2  		:damage 2
				  :skills [["fire" 6] ["lightning" 4] ["poison" 8]]}
		 :player {:health 10        :damage 2           :defense 0
				  :inventories []   :location 0			:check-around 3
				  :tick 0           :seen #{"0"}}
		 :items  {armor "ARMOR"     					sword "SWORD"
				  fire-resist "fire-resist"				lightning-resist "lightning-resist"
				  poison-resist "poison-resist"}
		}))

(defn move-boss
	"move boss to one of its neighboring room randomly, 'update' state, and return the new state"
	[state]
	(println "The BOSS moved\n")
	(let [possible-locs (-> state :boss :location maze)
		  idx           (rand-int (count possible-locs))]
		  (assoc-in state [:boss :location] (possible-locs idx))
	))

(defn boss-is-nearby
    "input curr state, if boss is nearby, return true and print out warning, return false otherwise"
	[state]
	(let [res (contains? (into #{} (-> state :player :location maze)) (-> state :boss :location))]
		(when res (println "  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \n"
						   	"  WARNING: The boss is in a nearby room  \n"
						   	" ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \n"))
		res))

(defn check-around 
   [state]
   (let [curr-loc (-> state :player :location)
     	 nearby (maze curr-loc)
   		 check-times (-> state :player :check-around)
   		 used-check (assoc-in state [:player :check-around] (dec check-times))
      	 boss-loc (-> state :boss :location)]

        (if (>= 0 check-times)
        		(println "\n###You do not have the ability to check-around.###\n")
      			(println "\n###Checking nearby locations...###\n\n"
					(for [room nearby] 
						[(str "#"room": " 
						 (if ((into #{} (keys (state :items))) room)
				  			(str ((state :items) room) " ")
                            "")
						 (if (== boss-loc room) "boss" ""))])"\n"))
   		used-check))

(defn what-is-found
    "determine what new items are found, 'update' state, return the new state"
    [state]
    (let [remaining-items-set (into #{} (keys (state :items)))
          curr-room (-> state :player :location)
          curr-invt (-> state :player :inventories)]
		;; if curr-room have something
		;; update player invts and possibly defense or damage, remove room-item from state item
        (if (remaining-items-set curr-room)
            (let [room-item ((state :items) curr-room)
				  add-invt (assoc-in state [:player :inventories] (conj curr-invt room-item))]
                (println "You found a new item!")  
				(println "**" ((item-description room-item) :name) "**\n")
				(println ((item-description room-item) :desc))
                (cond (= room-item "ARMOR") 
							(let [add-invt-def (assoc-in add-invt [:player :defense] 2)]
								(println "Your defense increased from 0 -> 2")
								(assoc-in add-invt-def [:items] (dissoc (state :items) curr-room)))
					  (= room-item "SWORD") 
							(let [add-invt-dmg (assoc-in add-invt [:player :damage] 4)]
								(println "Your damage increased from 2 -> 4")
								(assoc-in add-invt-dmg [:items] (dissoc (state :items) curr-room)))
					  :else 
					  		(assoc-in add-invt [:items] (dissoc (state :items) curr-room))))
            (do (println "No item found in this room") 
				state)
		)))

(defn meet-boss
	"return true if boss and the player are in the same cell, false otherwise"
	[state]
	(let [player-pos (-> state :player :location)
		  boss-pos (-> state :boss :location)]
		  (= player-pos boss-pos)))

(defn print-room-desc
"player check the information of the current room, print the description of the room,
return nothing"
[state]
(let [curr-room (-> state :player :location)]
		((room-description curr-room) :desc)))

(defn player-move
	"move player to the next location depending on player's input, return the new state"
	[state]
	(println "What do you want to do? [R]oom-Description/[S]een-rooms/[C]heck-around/[M]ove/[Q]uit")
	(let [choice (read-line)
      	  tick (-> state :player :tick)]
		(cond (or (= choice "M" ) (= choice "m")) 
					(let [avai (-> state :player :location maze)]
						(println "available next steps are:" avai)
						(println "please enter the number of the place you want to go:")
						(loop [room-choice (read-line)]
							(if ((into #{} (map str avai)) room-choice)
								(let [after-seen (assoc-in state [:player :seen] (conj (-> state :player :seen) room-choice))
									  after-tick (assoc-in after-seen [:player :tick] (inc tick))
									  ret-state (assoc-in after-tick [:player :location] (Integer/parseInt room-choice))]
									  (println (str "\n[Your choice is #" room-choice "] \n\n---------Map--------"))
									  (if ((-> state :player :seen) room-choice)
											(println (str "You are now in *" ((room-description (Integer/parseInt room-choice)) :name)"*\n\n------Inventory------"))
									   		(println (str "You found a new place! \n" (print-room-desc ret-state)"\n\n------Inventory------")))
									  ret-state)
								(do (println "Please key in a valid room number")
									(recur (read-line))) )))
			  (or (= choice "Q") (= choice "q"))  
					(do (println "Thanks for playing!")
						(System/exit 0))
			  :else (cond (or (= choice "R") (= choice "r"))
								(do (println (str "\nDescription: "(print-room-desc state)"\n"))
									(player-move state))
						  (or (= choice "S") (= choice "s"))
								(do (println (str "\nSeen rooms: "(-> state :player :seen)"\n"))
									(player-move state))
         				  (or (= choice "C") (= choice "c"))
          						(player-move (check-around state))
						  :else (do (println "Please input a valid key") 
									(player-move state)))
		)))

(defn fight-status
	"print out some information for player for reference"
	[state]
	(let [player-hp (-> state :player :health)
		player-damage (-> state :player :damage)
		player-defense (-> state :player :defense)
		player-invts (-> state :player :inventories)
		boss-hp (-> state :boss :health)]
		(println "\n"
		"############################## PLAYER status ##############################\n"
		"  [hp]" player-hp "\t\t[damage]" player-damage "\t\t[defense]" player-defense "\n"
		"  [inventories]" player-invts "\n"
		"############################### BOSS status ###############################\n"
		"  [hp]" boss-hp "\n"
		"###########################################################################\n")))

(defn boss-rand-attack
	"boss attack player randomly, return vect [skill-name, damage]"
	[state]
	(let [skills-num (count (-> state :boss :skills))
		  rand-pick (rand-int (+ skills-num 1))]
		(cond (= rand-pick skills-num)
					(do (println "BOSS Normal Attack\n")
						["normal" (-> state :boss :damage)])
			  :else
					(let [action ((-> state :boss :skills) rand-pick)]
						(println "BOSS used" (action 0))
						[(action 0) (action 1)])
		)))

(defn player-take-damage
	"player take damage according to defense and damage"
	[state skill-damage]
	(let [dmg (skill-damage 1)
			player-hp (-> state :player :health)
			player-def (-> state :player :defense)]
			(println "You took" (- dmg player-def) "damage")
			(assoc-in state [:player :health] (- player-hp (- dmg player-def)))))

(defn use-inventory
	"player use inventory AND take damage, return new state accordingly"
	[state skill-damage]
	(let [invts (-> state :player :inventories)
		  avai (vec (filter #(and (not (= % "ARMOR")) (not (= % "SWORD"))) invts))
		  idx-avai (for [idx (range (count avai))] 
		  			  	[(str idx ":") (avai idx)])
		  skl (skill-damage 0)]
		(if (<= (count avai) 0)
			(do (println "You don't have any inventories to use")
				(player-take-damage state skill-damage))
			(do (println "What item in your inventory do you want to use?")
				(println "Available items are" idx-avai)
				(println "Please key in the index:")
				(loop [choice (read-line)]
					(cond ((into #{} (map str (range (count avai)))) choice)
							(let [invt-choice ((into {} idx-avai) (str choice ":"))]
								(println "You used" invt-choice)
								(if (= skl "normal")
									(player-take-damage state skill-damage)
									(if (= (str skl "-resist") invt-choice)
										(do (println "Nice parry, you are intact in this attack!")
											state)
										(player-take-damage state skill-damage))))
						  :else
							(do (println "please input a valid index")
								(recur (read-line))))
				)))))

(defn attack-the-boss
	"Deal damage to the boss, according to the player's damage level, return new state"
	[state]
	(let [player-dmg (-> state :player :damage)
		  boss-hp (-> state :boss :health)]
		(println "You dealt" player-dmg "damage to the BOSS")
		(assoc-in state [:boss :health] (- boss-hp player-dmg))))

(defn player-response
	"player response boss attack with certain action, according to the input,
		this function will also calculate the damage boss and player take, return new state"
	[state skill-damage]
	(println "The BOSS attacked you, what do you want to do?")
	(println "You can take up to two actions:")
	(println "    [U]se inventory items/[A]ttack the boss/[B]oth")
	(loop [choice (read-line)]
		(cond (or (= choice "U") (= choice "u"))
					(use-inventory state skill-damage)
			  (or (= choice "A") (= choice "a"))
					(let [after-attack (attack-the-boss state)]
						(player-take-damage after-attack skill-damage))
			  (or (= choice "B") (= choice "b"))
					(let [after-attack (attack-the-boss state)]
						(use-inventory after-attack skill-damage))
			  :else
					(do (println "Please input a valid key") 
					(recur (read-line))))) )

(defn game-over
	"return true if game over, false otherwise"
	[state]
	(cond (<= (-> state :boss :health) 0)
				(do (println "You Win!")
					true)
		  (<= (-> state :player :health) 0)
				(do (println "You Lose!")
					true)
		  :else false))

(defn fight-boss
    "this function is called when player and boss are in the same cell"
    [state]
	(println "\n"
		"=======================================\n"
		">>>>>  THE FINAL FIGHT IS COMING  <<<<<\n"
		"=======================================")
	(println "Are You Ready? [Y]es/[N]o")
	(let [ready (read-line)]
		(if (or (= ready "Y") (= ready "y")) 
			(println "ONLY WINNER SURVIVES... GOOD LUCK!") (println "There is no way back now ... FIGHT UNTIL YOU DIE!"))
			(Thread/sleep 1500))
	(fight-status state)
	
	;; FIGHT HERE
	(loop [curr-state state]
		(let [new-state (player-response curr-state (boss-rand-attack curr-state))]
			(fight-status new-state)
			(if (game-over new-state)
				(do (println "Thanks for playing!")
					(System/exit 0))
				(recur new-state)))) )

(defn play
	"play loop, helper function for main"
	[state]
	(println "==============================")
	(println "##   Welcome to the game!   ##")
	(println "==============================")
	(println "Press enter to continue...")
	(read-line)
	(loop [curr-state state]
		(let [after-boss-move (move-boss curr-state)]
			(boss-is-nearby after-boss-move)
			(let [after-find (what-is-found (player-move after-boss-move))]
				(println (str "Your inventory:" (-> after-find :player :inventories)"\n\n----Game Continues----"))
				(if (meet-boss after-find)
					(fight-boss after-find)
					(recur after-find)))
			)))

(defn main 
	"main function for playing the game"
	[]
	(play (new-game)))

(main)
