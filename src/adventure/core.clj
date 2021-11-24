(ns adventure.core
  (:gen-class))

;; player starts at 0
;; boss starts at 2
(def maze  {1 [2 5] 2 [1 4 6] 3 [4 7] 4 [2 11] 5 [1 6 8]
			6 [2 5 7 9] 7 [3 6 11] 8 [5 12 13] 9 [6 10 12] 10 [9 11 14]
			11 [4 7 10] 12 [8 9 14] 13 [8 14] 0 [10 12 13]})

(def maze-size (count maze))

(defn rand-unique
    "Pick a random number from 0 to `max-1` that is not in the set `exclude`.  Does not check for errors."
    [max exclude]
    (let [pick (rand-int max)]
        (if (exclude pick) (rand-unique max exclude) pick)))

(defn new-game []
	"Generate the initial state of the game, return the state"
	"contains information of boss, player, and items in the maze by mapping room# to item name"
  	(let [armor (rand-unique maze-size #{0})
		  fire-resist (rand-unique maze-size #{0 armor})
		  lightning-resist (rand-unique maze-size #{0 armor fire-resist})
		  poison-resist (rand-unique maze-size #{0 armor fire-resist lightning-resist})
		  heal-potion (rand-unique maze-size #{0 armor fire-resist lightning-resist poison-resist})
		  sword (rand-unique maze-size #{0 armor fire-resist lightning-resist poison-resist heal-potion})
		  ]
		{:boss   {:health 20  		:location 2  		:damage 2
				  :skills [["fire" 6] ["lightning" 4] ["poison" 8]]}
		 :player {:health 10        :damage 1           :defense 0
				  :inventories []   :location 0			:look-around 3
				  :tick 0           :seen 0}
		 :items  {armor "ARMOR"     					sword "SWORD"
				  fire-resist "fire-resist"				lightning-resist "lightning-resist"
				  poison-resist "poison-resist"			heal-potion "heal-potion"}
		}))

(defn move-boss
	"move boss to one of its neighboring room randomly, 'update' state, and return the new state"
	[state]
	(println "BOSS moved")
	(let [possible-locs (-> state :boss :location maze)
		  idx           (rand-int (count possible-locs))]
		  (assoc-in state [:boss :location] (possible-locs idx))
	))

(defn boss-is-nearby
    "input curr state, return true if boss is nearby"
	[state]
	(let [res (contains? (into #{} (-> state :player :location maze)) (-> state :boss :location))]
		(when res (println "The boss is in a nearby room"))
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
								(assoc-in add-invt-def [:items] (dissoc ((state :items) curr-room))))
					  (= room-item "SWORD") 
							(let [add-invt-dmg (assoc-in add-invt [:player :damage] 4)]
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
					(do (let [curr-pos (-> state :player :location)
						   	  avai (maze curr-pos)]
							(println "available next steps are:" avai)
							(println "please enter the room you want to go:"))
						(let [room-choice (read-line)]
							(println (str "Your choice is room #" room-choice))
							(assoc-in state [:player :location] room-choice)))
			  (or (= choice "Q") (= choice "q"))  
					(do (println "Thanks for playing!"))
			  :else ((do (println "Please key in M or Q") 
			  			  (player-move state)))) ))

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
		"############# PLAYER status #############\n"
		"  [hp]" player-hp "    [damage]" player-damage "    [defense]" player-defense " \n"
		"  [inventories]" player-invts "\n"
		"############## BOSS status ##############\n"
		"  [hp]" boss-hp "\n"
		"#########################################")))

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

;; (defn player-response
;; 	"player response boss attack with certain action, according to the input"
;; 	"input damage, return updated state"
;; 	[damage]
;; 	(println "The BOSS attacked you, what do you want to do?")
;; 	(println "You can take up to two actions:")
;; 	(println "    [U]se inventory items/[A]ttack the boss/[B]oth")
;; 	(loop [choice (read-line)]
;; 		(cond (or (= choice "U") (= choice "u"))
;; 					()
;; 			  (or (= choice "A") (= choice "a"))
;; 			  		()
;; 			  (or (= choice "B") (= choice "b"))
;; 			  		()
;; 			  :else
;; 			  		(do (println "Please key in U or A or B") 
;; 						(recur (read-line))) )))

(defn use-inventory
	"player use inventory item, calculate damage taken by player, return new state accordingly"
	[state skill-damage]
	(let [invts (-> state :player :inventories)
		  avai (vec (filter #(and (not (= % "ARMOR")) (not (= % "SWORD"))) invts))
		  idx-avai (for [idx (range (count avai))] [(str idx ":") (avai idx)])
		  skl (skill-damage 0)
		  dmg (skill-damage 1)
		  player-hp (-> state :player :health)
		  player-def (-> state :player :defense)]
	(if (<= (count avai) 0)
		(do (println "You don't have any inventories to use")
			(assoc-in state [:player :health] (- player-hp (- dmg player-def))))
		(do (println "What item in your inventory do you want to use?")
			(println "Available items are" idx-avai)
			(println "Please key in the index:")
			(loop [choice (read-line)]
				(cond ((into #{} (map str (range (count avai)))) choice)
						(let [invt-choice ((into {} idx-avai) (str choice ":"))]
							(println "You used" invt-choice)
							(if (= skl "normal")
								(assoc-in state [:player :health] (- player-hp (- dmg player-def)))
								(if (= (str skl "-resist") invt-choice)
									(do (println "Nice parry, you are intact in this attack!")
										state)
									(assoc-in state [:player :health] (- player-hp (- dmg player-def)))
								)))
					  :else
						(do (println "please key in a valid index")
							(recur (read-line))))) )
	)))

(defn attack-the-boss
	"Deal damage to the boss, according to the player's damage level, return new state"
	[state]
	(let [dmg (-> state :player :damage)
		  boss-hp (-> state :boss :health)]
		(println "You dealt" dmg "damage to the BOSS")
		(assoc-in state [:boss :health] (- boss-hp dmg))))

(defn fight-boss
    "this function is called when player and boss are in the same cell"
    [state]
	(println "\n"
		"=====================================\n"
		">>>>> THE FINAL FIGHT IS COMING <<<<<\n"
		"=====================================")
	(println "Are You Ready? [Y]es/[N]o")
	(let [ready (read-line)]
		(if (or (= ready "Y") (= ready "y")) 
			(println "ONLY WINNER SERVIVES ... GOOD LUCK!") (println "There is no way back now ... FIGHT UNTIL YOU DIE!"))
			(Thread/sleep 1500)
			(println "BOSS SPAWNING...")
			(Thread/sleep 2000))
	(fight-status state)
	
	;; TODO: FIGHT HERE
	(def new-state state)	;; place holder

	(cond (<= (-> new-state :boss :health) 0)
				(println "You Win!")
		  (<= (-> new-state :player :health) 0)
				(println "You Lose!")
		  :else (fight-boss new-state)))





(def state (new-game))

(println (use-inventory state ["poison" 8]))
