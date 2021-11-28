(ns adventure.core
  (:gen-class))

;; player starts at 0
;; boss starts at 2
(def maze  {0 [10 12 13] 1 [2 5] 	 2 [1 4 6] 	3 [4 7] 	4 [2 3 11]
			5 [1 6 8]	 6 [2 5 7 9] 7 [3 6 11] 8 [5 12 13] 9 [6 10 12]
			10 [0 9 11]  11 [4 7 10] 12 [0 8 9] 13 [0 8]})

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
				  :inventories []   :location 0			:look-around 3
				  :tick 0           :seen 0}
		 :items  {armor "ARMOR"     					sword "SWORD"
				  fire-resist "fire-resist"				lightning-resist "lightning-resist"
				  poison-resist "poison-resist"}
		}))

(defn move-boss
	"move boss to one of its neighboring room randomly, 'update' state, and return the new state"
	[state]
	(println "The BOSS moved")
	(let [possible-locs (-> state :boss :location maze)
		  idx           (rand-int (count possible-locs))]
		  (assoc-in state [:boss :location] (possible-locs idx))
	))

(defn boss-is-nearby
    "input curr state, if boss is nearby, return true and print out warning, return false otherwise"
	[state]
	(let [res (contains? (into #{} (-> state :player :location maze)) (-> state :boss :location))]
		(when res (println "  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \n"
						   	"  WARNING: The boss is in a nearby room \n"
						   	" ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \n"))
		res))

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
                (println "You found" room-item)     ;; TODO: add description for the item
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

(defn player-move
	"move player to the next location depending on player's input, return the new state"
	[state]
	(println "What do you want to do? [M]ove/[Q]uit")
	(let [choice (read-line)]
		(cond (or (= choice "M" ) (= choice "m")) 
					(let [avai (-> state :player :location maze)]
						(println "available next steps are:" avai)
						(println "please enter the room you want to go:")
						(loop [room-choice (read-line)]
							(if ((into #{} (map str avai)) room-choice)
								(do (println (str "Your choice is room #" room-choice))
									(assoc-in state [:player :location] (Integer/parseInt room-choice)))
								(do (println "Please key in a valid room number")
									(recur (read-line))) )))
			  (or (= choice "Q") (= choice "q"))  
					(do (println "Thanks for playing!")
						(System/exit 0))
			  :else (do (println "Please key in M or Q") 
			  			(player-move state)) )))

(defn check-around
	"player check around, print out all neighboring room, consume 1 look-around"
	[]
	)

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
		"###########################################################################")))

(defn boss-rand-attack
	"boss attack player randomly, return vect [skill-name, damage]"
	[state]
	(let [skills-num (count (-> state :boss :skills))
		  rand-pick (rand-int (+ skills-num 1))]
		(cond (= rand-pick skills-num)
					(do (println "BOSS Normal Attack")
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
							(do (println "please key in a valid index")
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
					(do (println "Please key in U or A or B") 
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
			(println "ONLY WINNER SERVIVES ... GOOD LUCK!") (println "There is no way back now ... FIGHT UNTIL YOU DIE!"))
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
	(println "Press any button to continue...")
	(read-line)
	(loop [curr-state state]
		(let [after-boss-move (move-boss curr-state)]
			(boss-is-nearby after-boss-move)
			(let [after-find (what-is-found (player-move after-boss-move))]
				(println "Your inventories:" (-> after-find :player :inventories))
				(if (meet-boss after-find)
					(fight-boss after-find)
					(recur after-find)))
			)))

(defn main 
	"main function for playing the game"
	[]
	(play (new-game)))

(main)
