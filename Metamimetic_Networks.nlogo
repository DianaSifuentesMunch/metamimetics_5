extensions [ nw ]

;Order of commands for outputs: 
;checkout order when printing sw test (due to dissaperance of turtles) 
;export-network
;export-data

;take measures in noisy settings? 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; To Go ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
;uncomment to change dynamically on widget
set Strength-of-Dilemma *-strength-of-dilemma
set inicoop *-inicoop
set replacement? *-replacement?



ask turtles [interact]

decision-stage
learn-stage

;uncomment to change dynamically on widget
ask turtles [establish-color]
ask turtles [set-faces]

ask turtles [set satisfaction2 satisfaction-2]

set-outputs            


;uncomment to change dynamically on widget
my-update-plots
reset-change
if replacement? [replacement]
ask turtles [set age age + 1]

tick
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              Go Routines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to reset-change
ask turtles   [
              set rule? false
              set behavior? false
              ]
end

to set-faces
ifelse am-i-the-best?   [set shape "face happy"]
                        [set shape "face sad"]                          
end

to interact  ;; calculates the agents' payoffs for Prisioner's Dilema.
let total-cooperators count link-neighbors with [cooperate?]
set inst-score 0
ifelse cooperate?
    [set inst-score (total-cooperators * (1 - Strength-of-Dilemma) / count link-neighbors)]
    [set inst-score ((total-cooperators + (count link-neighbors - total-cooperators) * Strength-of-Dilemma ) / count link-neighbors)]  
set last-score score
set score inst-score
end


to-report am-i-the-best? ;; reports true if the agent is the best in its neighborhood
 let test false  
 if (rule = 1) and  (score >= [score] of max-one-of link-neighbors [score]  * 0.99)  [set test true]
 if (rule = 2) and  (score <= [score] of min-one-of link-neighbors [score] * 1.01)     [set test true]
 if (rule = 3) and  (member? rule majority-rules)                                      [set test true]
 if (rule = 4) and  (member? rule minority-rules) and not all? (link-neighbors) [rule = 4]  [set test true]    
 report test
end

to decision-stage
ask turtles [
  
  let satisfaction  am-i-the-best? ;  
;  if error_on_satisfaction
;  [
;  if random-float 1 <= copy-error-rule ;
;     [set satisfaction not am-i-the-best?]
;  ]

  
if not satisfaction
[
;if random-float 1 <= theta_2 ;only some agents will be allowed to change rule
;      [
      set rule? true 
      set behavior? true
;      ]
       
; if random-float 1 <= theta_1      
;      [set behavior? true]
]

if age < 15 
    [set rule? false]

]
end

to learn-stage
ask turtles 
[
if rule? [select-rule]
if behavior? [select-behavior]
]
end


to-report is-my-rule-the-best? ;; reports true if the agent's rule is used by any of the best valuated agents in its neighborhood (according with its rule) and false otherwise
  let test false
  ifelse am-i-the-best? [set test true][
  if member? rule [rule] of best-elements [set test true] 
  ]
  report test
end

; choose strategy if your rule is not the best, if innovate? choose rule if you are unsatisfied
to select-rule              
 ;  ifelse not is-my-rule-the-best?   
   if not is-my-rule-the-best?   
   [copy-strategy (one-of best-elements)]         
   
;   [if not am-i-the-best? and member? rule [rule] of best-elements and innovate? ;stuck agent will innovate with probability error-copy
;       [copy-strategy (one-of best-elements)]
;   ]
end



to copy-strategy [temp-agent]
;;;RULE STEP
;ifelse random-float 1.0 > copy-error-rule ; some agents do the right thing
;       [
       set rule [rule] of temp-agent
;         set theta_1 [theta_1] of temp-agent       
;         set theta_2 [theta_2] of temp-agent 
;         if Copy-Thetas? [
;         set theta_1 add-noise "theta_1" Transcription-Error
;         set theta_2 add-noise "theta_2" Transcription-Error
;                         ]
;       ]     
;       [set rule random 4 + 1 ] ;do a random thing
;       
       set n-changes (n-changes + 1)
       set rule? false
       set time-rule age

             
end



to select-behavior 
;ifelse random-float 1 > copy-error-behavior ;only some agents do the right thing 
;       [ 
       if (rule = 1) or (rule = 2) [set cooperate? [cooperate?] of one-of best-elements ]
       if  rule = 3                 [set cooperate? majority-behavior]         
       if rule = 4                 [set cooperate? not majority-behavior]
;       ]
;      [
;      ifelse random-float 1.0 < .5  [set cooperate? true] [set cooperate? false] ;choose random behaviour
;      ]
set behavior? false
set time-behavior age
end 


to-report majority-behavior
  let mylist [cooperate?] of (turtle-set link-neighbors self)
  report one-of modes mylist
end

to-report satisfaction-2
let sat2 0
let myset (turtle-set link-neighbors self)
 
if rule = 1 [
            ifelse abs(max [score] of myset - min [score] of myset) = 0
            [set sat2 1]
            [set sat2  ( (score  - min [score] of myset ) / (max [score] of myset - min [score] of myset))] 
            ]                          

if rule = 2 [
            ifelse   (max [score] of myset - min [score] of myset) = 0
            [set sat2 1] 
            [set sat2  ( ( max [score] of myset - score  ) / ( max [score] of myset - min [score] of myset ))] 
            ]              

if rule = 3 [
            let my-frequency ( count link-neighbors with [rule = 3] + 1 ) / (count link-neighbors + 1)
            ifelse abs(min-frequency - max-frequency) = 0 
                                                         [set sat2 1]
                                                         [set sat2  (my-frequency - min-frequency) / (max-frequency - min-frequency)]
            ]

if rule = 4 [
            let my-frequency (count link-neighbors with [rule = 4]  + 1) / ( count link-neighbors + 1)
            ifelse abs( max-frequency - min-frequency ) = 0 
                                                       [set sat2 1]
                                                       [set sat2  ( max-frequency - my-frequency ) / (  max-frequency - min-frequency)]
            ]

report sat2
end


to-report majority-rules  ;; reports a set with the number of the most frequent rules in agent's neighborhood (agent included)
                          ;; be careful when use in an ask cycle as the command is applied to "self"
  let mylist [rule] of (turtle-set link-neighbors self)
  set mylist modes mylist
  report mylist
end

to-report minority-rules ;; reports a set with the number of the less frequent rules in agent's neighborhood (agent included)
                         ;; be careful when use in an ask cycle as the command is applied to "self"
  let mylist_1 [rule] of (turtle-set link-neighbors self)
  let mylist []
  let j 1
  while [empty? mylist] [
  let i 1
  repeat 4 [
    if length filter [? = i] mylist_1 = j  [set mylist lput i mylist] 
    set i i + 1
    ]
  set j j + 1
  ] 
  report mylist
end


to-report best-elements ;; report a list with the agents with the best performance according to agents  
  let myset (turtle-set link-neighbors self)
  if rule = 1 [set myset myset with [score >= [score] of max-one-of myset [score] * 0.99]]
  
  if rule = 2 [set myset myset with [score <= [score] of min-one-of myset [score] * 1.01]]
  
  if rule = 3 [
              let rules-list majority-rules
              set myset myset with [member? rule rules-list]
              ] 
  if rule = 4 [
              let rules-list minority-rules
              if not empty? rules-list [
                                        set myset myset with [member? rule rules-list]
                                       ]  
              ]
  
  report myset
end  

to-report add-noise [value noise-std]
      let epsilon random-normal 0.0 noise-std
      if ( epsilon <= -100 )
      [ set epsilon -99] 
      let noisy-value runresult value * 100 / ( 100 + epsilon )
      if (noisy-value > 1) [set noisy-value 1]
      if (noisy-value < 0) [set noisy-value 0]     
      report noisy-value
end


to-report min-frequency
let l item 0 minority-rules
report count (turtle-set link-neighbors self) with [rule = l] / (count link-neighbors + 1)
end

to-report max-frequency
let l item 0 majority-rules
report count (turtle-set link-neighbors self) with [rule = l] / (count link-neighbors + 1)
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Life Distributions  ;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to init-age-USA2010 ;;Population fraction for ages according data colected
                                 ;By Lindsay M. Howden and Julie A. Meyer in
                                 ;Age and Sex Composition: 2010 Census briefs
                                 ;Reported fractions have an interval of 5 years starting from 0 until 100 years
  let census-dist (list 0.0654 0.0659 0.0670 0.0714 0.0699 0.0683 0.0647 0.0654 0.0677 0.0735 0.0722 0.0637 0.0545 0.0403 0.0301 0.0237 0.0186 0.0117 0.0047 0.0012 0.0002)
  ask turtles [
    let temp-init random 21
    while [random-float 1 > item temp-init census-dist][set temp-init random 21]
    set age (temp-init * 5) + random 5
    ]
end

to set-life-distribution-USA2010 ;;Life expectation for ages according data colected by the Centers for Disease Control
                                 ;and Prevention’s National Center for Health Statistics (NCHS) USA 2010
                                 ;Murphy, Xu, and Kochanek 'Deaths: preliminary data 2010' National Vital Stat. Reports 60-4
                                 ;Reported ages have an interval of 5 years starting from 0 until 100 years

  set life-distribution (list 78.7 74.3 69.3 64.4 59.5 54.8 50.0 45.3 40.6 36.0 31.5 27.2 23.1 19.2 15.5 12.2 9.2 6.6 4.7 3.3 2.4) 
end

to replace  
;    ifelse random-float 1.0 < 0.5 [set cooperate? true][set cooperate? false]        
    set age 0
    set rule? false
    set behavior? false
;   set move? false
    set rule (random 4) + 1 
    ;    move-to one-of (patch-set patches with [not any? turtles-here] patch-here)
    set shape "face sad"
    set size 1
    set satisfaction2 1
    ifelse random-float 1.0 < (inicoop / 100)
        [set cooperate? true]
        [set cooperate? false]
    establish-color
    set score 0.0
    set rule? false
    set behavior? false
     ;set move? false


;    set copy-error-rule     PER  
;    set copy-error-behavior PEB
;       
     
;ifelse Asynchronous-Updating?
;     [
;     ifelse random-init-u?
;       [
;       set theta_1 random-float 1.0
;       set theta_2 random-float 1.0
;       ]
;       [
;       set theta_1 Initial-prob-update-behavior
;       set theta_2 Initial-prob-update-rule
;       ]
;     ]
;;     [
;     set theta_1 1
;     set theta_2 1
;;     ]

set time-rule 0
set n-changes 0
set shuffled? false
end

to replacement
  ask turtles [    
     let index1 floor age / 5
     let index2 floor (age + 1) / 5
     if index1 > 20 [set index1 20]
     if index2 > 20 [set index2 20]
     
     let ex1 item index1 life-distribution
     let ex2 item index2 life-distribution
     
     let prob-death 1 - (ex1 / (ex2 + 1))
     
     
     ifelse  random-float 1  < prob-death 
       [
       set-info-death
       replace
       set shape "target" 
       ]
       [set age age + 1]
  ]
end   

to set-info-death
if ticks > 100
[
set n-changes-list lput n-changes n-changes-list
set time-rule-list lput time-rule time-rule-list
set rule-at-death-list lput rule rule-at-death-list
set time-behavior-list lput time-behavior time-behavior-list
set age-at-death-list lput age age-at-death-list
set nodes-list lput ([who] of self ) nodes-list
]
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Layout  ;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Layout code citation
;Wilensky, U. (2005). NetLogo Preferential Attachment model. http://ccl.northwestern.edu/netlogo/models/PreferentialAttachment. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
;Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

;; resize-nodes, change back and forth from size based on degree to a size of 1
to resize-nodes
  ifelse all? turtles [size <= 1]
  [
    ;; a node is a circle with diameter determined by
    ;; the SIZE variable; using SQRT makes the circle's
    ;; area proportional to its degree
    ask turtles [ set size sqrt count link-neighbors ]
  ]
  [
    ask turtles [ set size 1 ]
  ]
end


to layout
  ;; the number 3 here is arbitrary; more repetitions slows down the
  ;; model, but too few gives poor layouts
  repeat 10 [
    ;; the more turtles we have to fit into the same amount of space,
    ;; the smaller the inputs to layout-spring we'll need to use
    let factor sqrt count turtles
    ;; numbers here are arbitrarily chosen for pleasing appearance
    layout-spring turtles links (1 / factor) (7 / factor) (1 / factor)
 ;  layout-spring (turtles) links 0.4 6 1

;    display  ;; for smooth animation
  ]
  ;; don't bump the edges of the world
  let x-offset max [xcor] of turtles + min [xcor] of turtles
  let y-offset max [ycor] of turtles + min [ycor] of turtles
  ;; big jumps look funny, so only adjust a little each time
  set x-offset limit-magnitude x-offset 0.1
  set y-offset limit-magnitude y-offset 0.1
  ask turtles [ setxy (xcor - x-offset / 2) (ycor - y-offset / 2) ]
end

to-report limit-magnitude [number limit]
  if number > limit [ report limit ]
  if number < (- limit) [ report (- limit) ]
  report number
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Topologies  ;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to setup-Topology
  if-else Topology = "Lattice"       [Create-Lattice]
    [
    if-else Topology = "Random"        [Create-Random-Network]
      [if-else Topology = "Small-World" [Create-Small-World]
          [if-else Topology = "Barabasi-Albert" [Create-Barabasi] [Create-Scale-Free]]                                                  
      ]
    ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lattice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to Create-Lattice 

;let size-lattice floor sqrt Num-Agents
;nw:generate-lattice-2d turtles links size-lattice size-lattice true 

ask patches [sprout 1] ;Num-Agents
ask turtles [create-links-with turtles-on neighbors]

;wrap world 
let rightcorner patches with   [pxcor = max-pxcor]
let leftcorner patches  with   [pxcor = min-pxcor]
let bottomcorner patches with  [pycor = min-pycor]
let topcorner patches with     [pycor = max-pycor] 

set rightcorner turtles-on rightcorner
set leftcorner turtles-on leftcorner
set bottomcorner turtles-on bottomcorner
set topcorner turtles-on topcorner

ask rightcorner [create-links-with leftcorner with [ pycor = [pycor] of myself ]]
ask rightcorner [create-links-with leftcorner with [ pycor = [pycor + 1] of myself]]
ask rightcorner [create-links-with leftcorner with [ pycor = [pycor - 1] of myself ]]

ask bottomcorner [create-links-with topcorner with [ pxcor = [pxcor] of myself ]]
ask bottomcorner [create-links-with topcorner with [ pxcor = [pxcor + 1 ] of myself ]]
ask bottomcorner [create-links-with topcorner with [ pxcor = [pxcor - 1] of myself  ]]


let corners (patch-set  patch min-pxcor min-pycor patch min-pxcor max-pycor patch max-pxcor max-pycor patch max-pxcor min-pycor)
ask turtles-on corners [create-links-with other turtles-on corners] 

ask links [set color black]

set success? true
;set average-path-length nw:mean-path-length 
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Random Network ;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to Create-Random-Network 
  ;; Make a circle of turtles
  create-turtles Num-Agents
  layout-circle sort turtles  (radius )

while [not success?]
[  ;; create links
  ask links [die]
  ask turtles [
    ;; each pair of turtles is only considered once
  create-links-with turtles with [self > myself and
                                  random-float 1.0 < Connection-Probability]
              ]

;  nw:generate-random turtles links Num-Agents Connection-Probability
;  nw:set-context turtles links
  set success? do-calculations
]
 ask links [set color gray]
end


to Create-Measures-for-Small-World 

;;;for equivalent random network
set SWtest false
set gammaSW infinity * (-1)
set lambdaSW infinity * (-1)
set Sdelta infinity * (-1)
let mean-path-length false
;for equivalent random network with same degree distribution
create-equivalent-random-network-with-same-degree 
nw:set-context turtles links

set mean-path-length nw:mean-path-length
set equivalent-path-length mean-path-length
ask turtles [set node-clustering-coefficient nw:clustering-coefficient]
set equivalent-clustering-coefficient mean  [ node-clustering-coefficient ] of turtles
set equivalent-clustering-coefficient-2      global-clustering-coefficient

set gammaSW (clustering-coefficient-2 / equivalent-clustering-coefficient-2)
set lambdaSW (average-path-length / equivalent-path-length)
set Sdelta (gammaSW / lambdaSW) 
if Sdelta > 1 [set SWtest true]

;;;Create Equivalent Ring Lattice
;ask turtles [set color red]
;let Red-turtles turtles with [color = red ]
;nw:set-context Red-turtles links 
;set clustering-lattice global-clustering-coefficient
;set omega ( (equivalent-path-length-same-degree / average-path-length) - (clustering-coefficient-2 / clustering-lattice) )
end

to create-equivalent-ring-lattice-with-same-degree
print("Creating an Equivalent Ring Lattice with Same Degree")

ask turtles [set free-stubs degree]
layout-circle sort turtles (radius )
let current-turtle-1 nobody 
let total-neighbors 0 
let total-left 0
let total-right 0 
let new-right nobody 
let new-left nobody 
let free-turtles turtles

while [any? free-turtles]
[
set current-turtle-1 max-one-of turtles [free-stubs]
show [who] of current-turtle-1
set total-neighbors [degree] of current-turtle-1
set total-left floor (total-neighbors / 2)
set total-right ceiling (total-neighbors / 2)
if total-left + total-right != total-neighbors [print ("wrong count")]

if total-right > 0 [set new-right turtles-to-the-right total-right current-turtle-1]
if total-left  > 0 [set new-left turtles-to-the-left total-left current-turtle-1 ]

ask current-turtle-1 [
                     if is-turtle-set? new-right
                        [
                        create-links-with new-right [set color green]
                        set free-stubs (free-stubs - count new-right)
                        ask new-right [set free-stubs (free-stubs - 1)]
                        ]
                     if is-turtle-set? new-left
                        [
                        create-links-with new-left [set color green]
                        set free-stubs (free-stubs - count new-left)
                        ask new-left [set free-stubs (free-stubs - 1)]
                        ]
                     ]
set free-turtles turtles with [free-stubs > 0]
]
print(" finished") 
 ask links with [color = gray] [die]
end

to-report turtles-to-the-right [k current-turtle]
let n [who] of current-turtle
let turtles-right nobody 
let candidate-turtle nobody 
let i 1 
while [not is-turtle-set? turtles-right]
[ 
set candidate-turtle turtle ((n + i) mod Num-Agents )
if [free-stubs] of candidate-turtle > 0 
[if candidate-turtle != current-turtle [set turtles-right (turtle-set turtles-right candidate-turtle)]]

set candidate-turtle nobody
set i (i + 1)
]
while [count turtles-right < k ]
[
set candidate-turtle nobody
while [not is-turtle? candidate-turtle]
[ 
set candidate-turtle turtle ((n + i) mod Num-Agents )
if [free-stubs] of candidate-turtle > 0 
[if candidate-turtle != current-turtle [set turtles-right (turtle-set turtles-right candidate-turtle)]]
set i (i + 1)
]
] 
report turtles-right
end

to-report turtles-to-the-left [k current-turtle]
let n [who] of current-turtle
let turtles-left nobody 
let candidate-turtle nobody 
let i 1 
while [not is-turtle-set? turtles-left]
[ 
set candidate-turtle turtle ((n - i) mod Num-Agents )
if [free-stubs] of candidate-turtle > 0 
[if candidate-turtle != current-turtle [set turtles-left (turtle-set turtles-left candidate-turtle)]]
set candidate-turtle nobody
set i (i + 1)
]
while [count turtles-left < k ]
[
set candidate-turtle nobody
while [not is-turtle? candidate-turtle]
[ 
set candidate-turtle turtle ((n - i) mod Num-Agents )
if [free-stubs] of candidate-turtle > 0 
[if candidate-turtle != current-turtle [set turtles-left (turtle-set turtles-left candidate-turtle)]]
set i (i + 1)
]
] 
report turtles-left
end



to create-equivalent-random-network-with-same-degree
print("Creating an Equivalent Random Netowrk with Same Degree")
 let current-turtle-1 nobody
 let current-turtle-2 nobody
 let candidate-turtle-1 nobody
 let candidate-turtle-2 nobody
 let swap-turtles (turtle-set current-turtle-1 current-turtle-2 candidate-turtle-1 candidate-turtle-2)
 let rewire-count 0
 let candidates-2 nobody  
while [ rewire-count < ( n-links * 10 )  ]
 [
set current-turtle-1 nobody
set current-turtle-2 nobody
set candidate-turtle-1 nobody
set candidate-turtle-2 nobody
set swap-turtles (turtle-set current-turtle-1 current-turtle-2 candidate-turtle-1 candidate-turtle-2)
 
 while [count swap-turtles < 4]
 [
 set current-turtle-1 one-of turtles
 set candidate-turtle-1 one-of [link-neighbors] of current-turtle-1
 set current-turtle-2 one-of turtles with [not member? self (turtle-set current-turtle-1 candidate-turtle-1 ([link-neighbors] of candidate-turtle-1))]
 set candidates-2 turtles with [not member? self (turtle-set current-turtle-1 candidate-turtle-1 ([link-neighbors] of current-turtle-1 )) and member? self [link-neighbors] of current-turtle-2]
 set candidate-turtle-2 one-of candidates-2
 set swap-turtles (turtle-set current-turtle-1 current-turtle-2 candidate-turtle-1 candidate-turtle-2)
 ]
 
 ask link [who] of current-turtle-1 [who] of candidate-turtle-1 [die] 
 ask link [who] of current-turtle-2 [who] of candidate-turtle-2 [die] 
 ask current-turtle-1 [create-link-with candidate-turtle-2]
 ask current-turtle-2 [create-link-with candidate-turtle-1]
 set rewire-count (rewire-count + 1)

]
print("finished") 
end

;to create-equivalent-random-network
;;Create an equivalent random network
;create-Purples Num-Agents
;layout-circle turtles ( radius ) 
;set mean-path-length false
;set success? false
;set connected? false
;while[not connected?]
;[
;  ask links [die]
;  repeat n-links
;  [
;    ask one-of turtles [
;        ask one-of other turtles with [not link-neighbor? myself]
;        [create-link-with myself [set color violet]]
;                       ]
;  ]
;  set Purple-links links with [color = violet]
;  nw:set-context Purples Purple-links
;  set connected? true
;  set mean-path-length nw:mean-path-length
;  if not is-number? mean-path-length [set connected? false]
;]
;set equivalent-path-length mean-path-length
;ask Purples [set node-clustering-coefficient nw:clustering-coefficient]
;set equivalent-clustering-coefficient mean  [ node-clustering-coefficient ] of Purples
;set equivalent-clustering-coefficient-2      global-clustering-coefficient
;end




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Barabasi ;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to Create-Barabasi 
set success? true
  ;; Make a circle of turtles
create-turtles Num-Agents
layout-circle sort turtles  (radius )

while [not success?]
[  ;; create links
ask links [die]
nw:generate-preferential-attachment turtles links Num-Agents
nw:set-context turtles links
set success? do-calculations
]

ask links [set color gray]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Small World  ;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to Create-Small-World ;; taken from the netlogo library
create-turtles Num-Agents
layout-circle sort turtles (radius )
wire-rewire-them
end

to wire-them
ask links [die]
 let n 0
  while [n < Num-Agents]
  [;; make edges with the next "Initial-Neighbours"
  let i 1
  while [i <= ceiling ( Initial-Neighbours / 2 ) ]
  [
    ask turtle n [create-link-with turtle ((n + i) mod Num-Agents)
                  [set rewired? false]
                  ]
    set i i + 1
  ]
 set n (n + 1)
 ]
end

to wire-rewire-them

  ;; set up a variable to see if the network is connected
  set success? false

  ;; if we end up with a disconnected network, keep trying
  while [not success?] [
    ;; kill the old lattice, reset neighbors, and create new lattice    
    wire-them
    ask links [
      ;; whether to rewire it or not?
      if (random-float 1) < Rewiring-Probability
      [
        ;; "a" remains the same
        let node1 end1
        ;; if "a" is not connected to everybody
        if [ count link-neighbors ] of end1 < ( Num-Agents - 1)
        [
          ;; find a node distinct from node1 and not already a neighbor of node1
          let node2 one-of turtles with [ (self != node1) and (not link-neighbor? node1) ]
          ;; wire the new edge
          ask node1 [ create-link-with node2 [ set rewired? true ] ]
        set rewired? true
        ]
      ]
      
      ;; remove the old edge
      if (rewired?)
      [
        die
      ]
    ]
    ;; check to see if the new network is connected and calculate path length and clustering
    ;; coefficient at the same time
    nw:set-context turtles links
    set success? do-calculations
  
  ]
ask links [set color gray]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scale Free  ;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to Create-Scale-Free
set graphical? false
setup-sf-parameters
set success? false
let connected? false 

create-turtles Num-Agents [
  set xcor random-xcor
  set ycor random-ycor
  set degree infinity * (-1)
  ;layout-circle sort turtles (max-pxcor - 1)
  ]
let trials 1 
while [not success?] ;tells us if created network is connected
[
show "constructing"
while [not connected?] ;tells us if sequence has the potential to create a connected network 
[
while [not graphical?] ; verify if created sequence is graphical
[
create-degree-sequence
set graphical? verify-graphicality sequence Num-Agents
show "sequence"
]

if-else sum sequence >= 2 * (Num-Agents - 1) [set connected? true]
                                             [set graphical? false]
]
configuration-model ;construct network
;havel-hakimi
nw:set-context turtles links
set success? do-calculations ;is it connected?
if success? = false [set trials trials + 1 ] ;try again to make it connected

if trials > 30      [set success? false ;if sufficient trials, start again
                     set graphical? false  
                     set connected? false                              
                     set trials 1
                    ]
ask links [set color gray]
]
end

to assign-degrees-to-turtles [degree-sequence]
let i 0
foreach degree-sequence
[
ask item i (sort turtles)
 [
   set degree ?
   set free-stubs ?
 ]
set i ( i + 1)
]
end

to Havel-Hakimi ;;Havel Hakimi procedure constructs a graph from degree sequence, only produces a subset of all possible graphs with given sequence
let free-turtles turtles with [free-stubs > 0]
while [any? free-turtles]
[
set free-turtles turtles with [free-stubs > 0]
let current-turtle max-one-of free-turtles [free-stubs]
let forbiden-turtles [link-neighbors] of current-turtle
ask current-turtle [set free-turtles other free-turtles with [not (member? self forbiden-turtles)]]

while[ [free-stubs] of current-turtle > 0]
[
let link-turtle max-one-of free-turtles [free-stubs]
ask current-turtle [create-link-with link-turtle]
ask (turtle-set current-turtle link-turtle) [set free-stubs (free-stubs - 1)]
ask link-turtle [set free-turtles other free-turtles]
]
]
layout-radial turtles links max-one-of turtles [betweenness-centrality]
end

to configuration-model ;;star constrained graphicality configuration model more efficient, 
                       ;;produces greater variety of graphs. The probability of producing a certain graph can be calculated for ensemble values

assign-degrees-to-turtles sequence
clear-links

let value -1 
let id -1

let residual-sequence sequence
let possible []

let current-turtle turtle (position (max residual-sequence) residual-sequence)
let free-turtles turtles with [free-stubs > 0]
let forbiden-turtles [link-neighbors] of current-turtle
let link-turtle one-of turtles

let contradiction? false 

while [any? free-turtles and not contradiction? ]
[
set current-turtle turtle (position (max residual-sequence) residual-sequence)
set forbiden-turtles [link-neighbors] of current-turtle
set free-turtles turtles with [free-stubs > 0]
ask current-turtle [set free-turtles other free-turtles with [ not (member? self forbiden-turtles)]]

while [ [free-stubs] of current-turtle > 0]
[

ifelse (length filter [? > 0] residual-sequence) = 2 
        and 
        (sum residual-sequence = 2)[set link-turtle one-of free-turtles                                   
                                    ]
                                 
                                   [
                                   set possible (possible-connections current-turtle free-turtles residual-sequence)
                                   if-else empty? possible
                                   [set contradiction? true];start construction again. 
                                   [set link-turtle turtle (one-of possible)]
                                   ]

ask current-turtle [create-link-with link-turtle]
ask (turtle-set link-turtle current-turtle) [set free-stubs (free-stubs - 1)]
ask link-turtle [set free-turtles other free-turtles]

set id [who] of current-turtle
set value (item id residual-sequence)
set residual-sequence replace-item id residual-sequence (value - 1) 

set id [who] of link-turtle
set value (item id residual-sequence)
set residual-sequence replace-item id residual-sequence (value - 1) 
]
]
end


to-report possible-connections [current-turtle free-turtles residual-sequence]
let final-list n-values Num-Agents [false]
let candidates []
let subsequence residual-sequence
let value -1
let id -1

foreach sort free-turtles
[
set subsequence residual-sequence

set id [who] of current-turtle
set value (item id residual-sequence)
set subsequence replace-item id subsequence (value - 1)

set id [who] of ?
set value (item id residual-sequence)
set subsequence replace-item id subsequence (value - 1)

set subsequence reverse sort subsequence
set subsequence filter [? > 0 ] subsequence

ifelse length subsequence = 2 and sum subsequence = 2 [set final-list replace-item ([who] of ?) final-list true] 
[
 ifelse item 0 subsequence >= length(subsequence) [set final-list replace-item ([who] of ?) final-list false] 
 [
   set final-list replace-item ([who] of ?) final-list (verify-graphicality subsequence length subsequence)
 ]
]

]

set candidates (positions final-list true)
report candidates
end

to-report positions [a-list value] 
  let ids n-values (length a-list) [?] 
  let ids-values map [list ? item ? a-list] ids 
  report map [first ?] filter [item 1 ? = value] ids-values 
end 


to-report verify-graphicality [seq length-sequence]
; k goes from 0 to length sequence - 1
let x_k n-values (length-sequence) [false]
let seqqx n-values (length-sequence) [?]
set seqqx  map [? + 1] seqqx 

let k_star 0
let flag false 

let i (-1)

if-else max seq > (length-sequence - 1) [set flag false]
[if-else (sum seq) mod 2 != 0 [set flag false]
[

;set x_k replace-item 0 x_k length-sequence
while [not flag]
[
;;find position of min index i such that sequence < seqqx(i)
let pos item (i + 1) seqqx
set pos max-position pos seq
set x_k replace-item (i + 1) x_k pos
if item (i + 1) x_k < (i + 1)[set flag true]
set i (i + 1)
]

let j 0
set flag false
while [j < (length x_k ) and not flag]
[
if item j (x_k) < (j + 1) 
[
set k_star j
set flag true 
]
set j (j + 1)
]


let k 0
let L_k item k seq
let R_k (length-sequence - 1)
set flag false 
set k 1

while [not flag and k <= (length-sequence - 1)]
[
;;;show k
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
set L_k (L_k + item k seq)

if-else k < k_star
[set R_k (R_k + (item k x_k) - 1)]
[set R_k (R_k + 2 * k - (item k seq))]

if L_k > R_k [set flag true]
set k (k + 1)
]

set flag (not flag)
]]

report flag
end


to-report max-position [number seq]
let count-positions 0
let max-iter (length seq)
let flag false 

while [ count-positions <  max-iter and not flag ]
[
;show count-positions
if-else (item count-positions seq) < number
[set flag true]
[set count-positions (count-positions + 1)]
]
report (count-positions) 
end




to  create-degree-sequence  ;create sequence with sum of degrees even
let parity 1
while [parity mod 2 != 0]
[
;create uniform
set uniform n-values (Num-Agents) [random-float 1]
set sequence []
foreach uniform [set sequence lput (create-one-degree ?) sequence]
set parity sum sequence
]
set sequence (sort sequence)
set sequence (reverse sequence)
end


to-report create-one-degree [R]
let x 0
let S (item x p_k)/ Z
while [S < R]
[
set x (x + 1)
set S (S + ((item x p_k) / Z))
]
report (x + 1)
end

to setup-sf-parameters ;set seq p_k (1, nodes-1,1)^(-g)
set p_k n-values (Num-Agents - 1 ) [?]
set p_k map [? + 1] p_k 
set p_k map [? ^ (- Scale-Free-Exponent)] p_k
set Z sum p_k
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Network Computations ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Connected Network? and Clustering 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; do-calculations reports true if the network is connected, finds path lengths 

to-report do-calculations
  
  let connected? true  
  
  ;let num-connected-pairs sum [length remove false (remove 0 distance-from-other-turtles)] of turtles
  ;;; In a connected network on N nodes, we should have N(N-1) measurements of distances between pairs,
  ;;; and none of those distances should be infinity.
  ;ifelse ( num-connected-pairs != (Num-Agents * (Num-Agents - 1) ))
  ;[set average-path-length infinity
  ; set connected? false
  ;]
  ;[
  let mean-path-length nw:mean-path-length
  ;]
  ;;find the clustering coefficient and add to the aggregate for all iterations  
  ;; report whether the network is connected or not
if not is-number? mean-path-length [set connected? false]
report connected?
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Path Length?  ;;;;;;;;;;;;;;;;;;;NOT USED ANYMORE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clustering computations ;;; SUBSTITUTED BY Function in LIBRARY NW::: added a new way of computing (yields different value) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;to find-clustering-coefficient
;  ifelse all? turtles [count link-neighbors <= 1]
;  [
;    set clustering-coefficient 0
;  ]
;  [
;   ask turtles [set node-clustering-coefficient nw:clustering-coefficient]
;   let total 0
;   set total sum [node-clustering-coefficient] of turtles 
;   set clustering-coefficient total / count turtles with [count link-neighbors > 1]
;  ]
;end



to-report global-clustering-coefficient
  let closed-triplets sum [ nw:clustering-coefficient * count my-links * (count my-links - 1) ] of turtles
  let triplets sum [ count my-links * (count my-links - 1) ] of turtles
  report closed-triplets / triplets
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Open Mole Routines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report go-stop?
ifelse all? turtles [shape = "face happy"]
[report true]
[report false]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Declare Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 globals [
  cooperation-rate
  satisfaction-rate
  satisfaction-rate2
  mincc
  mindeg

  life-distribution


;IN THE NETWORK CONTEXT
  clustering-coefficient               ;; average of clustering coefficients of all turtles
  average-path-length                  ;; average path length of the network
  network-density
  diameter
  n-links
  clustering-coefficient-2
  clustering-lattice
equivalent-path-length-same-degree
  SWtest
  gammaSW
  lambdaSW
  Sdelta
equivalent-path-length
equivalent-clustering-coefficient
equivalent-clustering-coefficient-2
omega 
original-degrees
Gray-links
;for connectivity
success?
;for small world and random.
infinity

;for scale-free
graphical?
sequence
p_k
Z
uniform


radius
;; Globals that come from the widget 
Topology                    ;type of graph
Num-Agents                  ;number of turtles
Connection-Probability      ;for random network
Initial-Neighbours          ;for small world network
Rewiring-Probability        
Scale-Free-Exponent         ;for scale-free network
Initial-Random-Types?       ;Agents' types initialization
Initial-Maxi-% 
Initial-Mini-% 
Initial-Conf-% 
Initial-Anti-% 
Strength-of-Dilemma         ;prisoner's dilemma
inicoop
Asynchronous-Updating?      ;updating type
;Initial-prob-update-rule
;Initial-prob-update-behavior
;random-init-u?
Transcription-error
Copy-Error-Random? 
PER
PEB

replacement?


n-changes-list 
time-rule-list 
rule-at-death-list 
time-behavior-list 
age-at-death-list
nodes-list

;OUTPUTS

  maxi-before-shuffle
  mini-before-shuffle
  conf-before-shuffle
  anti-before-shuffle



  maxi
  mini
  conf
  anti
  c-maxi  
  c-mini  
  c-conf  
  c-anti 
  cg-maxi 
  cg-mini 
  cg-conf 
  cg-anti
  sat-maxi
  sat-mini
  sat-conf
  sat-anti
  sd-sat-maxi
  sd-sat-mini
  sd-sat-conf
  sd-sat-anti
  
 mean-degree-1 
 mean-degree-2 
 mean-degree-3 
 mean-degree-4 
 
 max-degree-1 
 max-degree-2 
 max-degree-3 
 max-degree-4 
 
 

 
 mean-cc-1 
 mean-cc-2 
 mean-cc-3 
 mean-cc-4 
 
 mean-bc-1 
 mean-bc-2 
 mean-bc-3 
 mean-bc-4  
 
 mean-ec-1 
 mean-ec-2 
 mean-ec-3 
 mean-ec-4  
 
 mean-pr-1 
 mean-pr-2 
 mean-pr-3 
 mean-pr-4  
 
 mean-close-1 
 mean-close-2 
 mean-close-3 
 mean-close-4  
 
 mean-changes-1 
 mean-changes-2 
 mean-changes-3 
 mean-changes-4  
 sd-changes-1 
 sd-changes-2 
 sd-changes-3 
 sd-changes-4  
 
 max-tr-1 
 max-tr-2 
 max-tr-3 
 max-tr-4  
 
 min-tr-1 
 min-tr-2 
 min-tr-3 
 min-tr-4  


 max-cc-1
 max-bc-1
 max-ec-1
 max-pr-1
 max-cc-2
 max-bc-2
 max-ec-2
 max-pr-2
 max-cc-3
 max-bc-3
 max-ec-3
 max-pr-3
 max-cc-4
 max-bc-4
 max-ec-4
 max-pr-4
max-close-1
max-close-2
max-close-3
max-close-4

 
 sd-degree-1 
 sd-degree-2 
 sd-degree-3 
 sd-degree-4 
 
 sd-cc-1 
 sd-cc-2 
 sd-cc-3 
 sd-cc-4 
 sd-bc-1 
 sd-bc-2 
 sd-bc-3 
 sd-bc-4  
 sd-ec-1 
 sd-ec-2 
 sd-ec-3 
 sd-ec-4  
 sd-pr-1 
 sd-pr-2 
 sd-pr-3 
 sd-pr-4  
 sd-close-1 
 sd-close-2 
 sd-close-3 
 sd-close-4  

  
file.name


 mean-degree   
 median-degree  
 mean-cc   
 median-cc 
 mean-bc      
 median-bc    
 mean-ec      
 median-ec    
 mean-pr      
 median-pr    
 mean-close     
 median-close   

 mean-tr    
 median-tr  
 
 mean-changes    
 median-changes 


shuffled2? 
repetitions
]


turtles-own [
  cooperate?       ;; patch will cooperate
  rule             ;; patch will have one of four rules: 1=Maxi 2=mini 3=conformist 4=anticonformist  
 
  score            ;; score resulting from interaction of neighboring patches. It is dictated by the PD payoffs and the discount factor
  last-score
  inst-score
;  satisfaction
  satisfaction2
  age
  
  rule?
  behavior?
 
 
;  theta_1
;  theta_2

;  weighting-history

  copy-error-rule
  copy-error-behavior
  
;for network computations
distance-from-other-turtles

degree
free-stubs
node-clustering-coefficient
betweenness-centrality
eigenvector-centrality
page-rank
closeness-centrality
longest-path
mean-path

;time since last behavior change
time-rule
time-behavior
n-changes
neighbors-who
neighbors-type
;for outputs
shuffled?
]

links-own[
; for small-world 
 rewired?
  ]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Measures ;;;;;;;;;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



to set-outputs

set cooperation-rate count turtles with [cooperate?] / Num-Agents
set satisfaction-rate count turtles with [shape = "face happy"] / Num-Agents
set satisfaction-rate2  mean [satisfaction2] of turtles
  
  ;populations
  
let turtles-maxi turtles with [rule = 1]
let turtles-mini turtles with [rule = 2]
let turtles-conf turtles with [rule = 3]
let turtles-anti turtles with [rule = 4]


set maxi count turtles-maxi  
set mini count turtles-mini  
set conf count turtles-conf  
set anti count turtles-anti   
end

to set-final-outputs
  
set cooperation-rate count turtles with [cooperate?] / Num-Agents
set satisfaction-rate count turtles with [shape = "face happy"] / Num-Agents
set satisfaction-rate2  mean [satisfaction2] of turtles

let turtles-maxi turtles with [rule = 1]
let turtles-mini turtles with [rule = 2]
let turtles-conf turtles with [rule = 3]
let turtles-anti turtles with [rule = 4]


set maxi count turtles-maxi  
set mini count turtles-mini  
set conf count turtles-conf  
set anti count turtles-anti   


if count turtles-maxi > 0
[
  set sat-maxi       mean [satisfaction2] of turtles-maxi  
  set c-maxi         count turtles-maxi with [cooperate?] / count turtles-maxi
  set cg-maxi        count turtles-maxi with [cooperate?] / Num-Agents
  set mean-degree-1  mean [degree] of turtles-maxi  
  set max-degree-1   max [degree] of turtles-maxi
  set mean-cc-1      mean [node-clustering-coefficient] of turtles-maxi  
  set max-cc-1       max [node-clustering-coefficient] of turtles-maxi
  set mean-bc-1      mean [betweenness-centrality] of turtles-maxi  
  set max-bc-1       max [betweenness-centrality] of turtles-maxi 
  set mean-ec-1      mean [eigenvector-centrality] of turtles-maxi  
  set max-ec-1       max [eigenvector-centrality] of turtles-maxi
  set mean-pr-1      mean [page-rank] of turtles-maxi  
  set max-pr-1       max [page-rank] of turtles-maxi 
  set mean-close-1   mean [closeness-centrality] of turtles-maxi     
  set max-close-1    max [closeness-centrality] of turtles-maxi  
  set max-tr-1       max [time-rule] of turtles-maxi  
  set min-tr-1       min [time-rule] of turtles-maxi  
  set mean-changes-1 mean [n-changes] of turtles-maxi  
  
  if count turtles-maxi > 1
    [
      set sd-degree-1    standard-deviation [degree] of turtles-maxi  
      set sd-cc-1        standard-deviation [node-clustering-coefficient] of turtles-maxi  
      set sd-bc-1        standard-deviation [betweenness-centrality] of turtles-maxi  
      set sd-ec-1        standard-deviation [eigenvector-centrality] of turtles-maxi  
      set sd-pr-1        standard-deviation [page-rank] of turtles-maxi  
      set sd-close-1    standard-deviation [closeness-centrality] of turtles-maxi  
      set sd-sat-maxi    standard-deviation [satisfaction2] of turtles-maxi  
      set sd-changes-1   standard-deviation [n-changes] of turtles-maxi  
      ]]



if count turtles-mini > 0
[
  set sat-mini       mean [satisfaction2] of turtles-mini  
  set c-mini         count turtles-mini with [cooperate?] / count turtles-mini
  set cg-mini        count turtles-mini with [cooperate?] / Num-Agents
  set mean-degree-2  mean [degree] of turtles-mini  
  set max-degree-2   max [degree] of turtles-mini
  set mean-cc-2      mean [node-clustering-coefficient] of turtles-mini  
  set max-cc-2       max [node-clustering-coefficient] of turtles-mini
  set mean-bc-2      mean [betweenness-centrality] of turtles-mini  
  set max-bc-2       max [betweenness-centrality] of turtles-mini 
  set mean-ec-2      mean [eigenvector-centrality] of turtles-mini  
  set max-ec-2       max [eigenvector-centrality] of turtles-mini
  set mean-pr-2      mean [page-rank] of turtles-mini  
  set max-pr-2       max [page-rank] of turtles-mini 
  set mean-close-2   mean [closeness-centrality] of turtles-mini     
  set max-close-2    max [closeness-centrality] of turtles-mini  
  set max-tr-2       max [time-rule] of turtles-mini  
  set min-tr-2       min [time-rule] of turtles-mini  
  set mean-changes-2 mean [n-changes] of turtles-mini  
  
  if count turtles-mini > 1
    [
      set sd-degree-2    standard-deviation [degree] of turtles-mini  
      set sd-cc-2        standard-deviation [node-clustering-coefficient] of turtles-mini  
      set sd-bc-2        standard-deviation [betweenness-centrality] of turtles-mini  
      set sd-ec-2        standard-deviation [eigenvector-centrality] of turtles-mini  
      set sd-pr-2        standard-deviation [page-rank] of turtles-mini  
      set sd-close-2    standard-deviation [closeness-centrality] of turtles-mini  
      set sd-sat-mini    standard-deviation [satisfaction2] of turtles-mini  
      set sd-changes-2   standard-deviation [n-changes] of turtles-mini  
      ]]




if count turtles-conf > 0
[
  set sat-conf       mean [satisfaction2] of turtles-conf  
  set c-conf         count turtles-conf with [cooperate?] / count turtles-conf
  set cg-conf        count turtles-conf with [cooperate?] / Num-Agents
  set mean-degree-3  mean [degree] of turtles-conf  
  set max-degree-3   max [degree] of turtles-conf
  set mean-cc-3      mean [node-clustering-coefficient] of turtles-conf  
  set max-cc-3       max [node-clustering-coefficient] of turtles-conf
  set mean-bc-3      mean [betweenness-centrality] of turtles-conf  
  set max-bc-3       max [betweenness-centrality] of turtles-conf 
  set mean-ec-3      mean [eigenvector-centrality] of turtles-conf  
  set max-ec-3       max [eigenvector-centrality] of turtles-conf
  set mean-pr-3      mean [page-rank] of turtles-conf  
  set max-pr-3       max [page-rank] of turtles-conf 
  set mean-close-3   mean [closeness-centrality] of turtles-conf     
  set max-close-3    max [closeness-centrality] of turtles-conf  
  set max-tr-3       max [time-rule] of turtles-conf  
  set min-tr-3       min [time-rule] of turtles-conf  
  set mean-changes-3 mean [n-changes] of turtles-conf  
  
  if count turtles-conf > 1
    [
      set sd-degree-3    standard-deviation [degree] of turtles-conf  
      set sd-cc-3        standard-deviation [node-clustering-coefficient] of turtles-conf  
      set sd-bc-3        standard-deviation [betweenness-centrality] of turtles-conf  
      set sd-ec-3        standard-deviation [eigenvector-centrality] of turtles-conf  
      set sd-pr-3        standard-deviation [page-rank] of turtles-conf  
      set sd-close-3    standard-deviation [closeness-centrality] of turtles-conf  
      set sd-sat-conf    standard-deviation [satisfaction2] of turtles-conf  
      set sd-changes-3   standard-deviation [n-changes] of turtles-conf  
      ]]


if count turtles-anti > 0
[
  set sat-anti       mean [satisfaction2] of turtles-anti  
  set c-anti         count turtles-anti with [cooperate?] / count turtles-anti
  set cg-anti        count turtles-anti with [cooperate?] / Num-Agents
  set mean-degree-4  mean [degree] of turtles-anti  
  set max-degree-4   max [degree] of turtles-anti
  set mean-cc-4      mean [node-clustering-coefficient] of turtles-anti  
  set max-cc-4       max [node-clustering-coefficient] of turtles-anti
  set mean-bc-4      mean [betweenness-centrality] of turtles-anti  
  set max-bc-4       max [betweenness-centrality] of turtles-anti 
  set mean-ec-4      mean [eigenvector-centrality] of turtles-anti  
  set max-ec-4       max [eigenvector-centrality] of turtles-anti
  set mean-pr-4      mean [page-rank] of turtles-anti  
  set max-pr-4       max [page-rank] of turtles-anti 
  set mean-close-4   mean [closeness-centrality] of turtles-anti     
  set max-close-4    max [closeness-centrality] of turtles-anti  
  set max-tr-4       max [time-rule] of turtles-anti  
  set min-tr-4       min [time-rule] of turtles-anti  
  set mean-changes-4 mean [n-changes] of turtles-anti  
  
  if count turtles-anti > 1
    [
      set sd-degree-4    standard-deviation [degree] of turtles-anti  
      set sd-cc-4        standard-deviation [node-clustering-coefficient] of turtles-anti  
      set sd-bc-4        standard-deviation [betweenness-centrality] of turtles-anti  
      set sd-ec-4        standard-deviation [eigenvector-centrality] of turtles-anti  
      set sd-pr-4        standard-deviation [page-rank] of turtles-anti  
      set sd-close-4     standard-deviation [closeness-centrality] of turtles-anti  
      set sd-sat-anti    standard-deviation [satisfaction2] of turtles-anti  
      set sd-changes-4   standard-deviation [n-changes] of turtles-anti  
      ]]






set mean-degree    mean [degree] of turtles  
set median-degree  median [degree] of turtles

set mean-cc        mean [node-clustering-coefficient] of turtles  
set median-cc      median [node-clustering-coefficient] of turtles

set mean-bc        mean [betweenness-centrality] of turtles  
set median-bc      median [betweenness-centrality] of turtles 

set mean-ec        mean [eigenvector-centrality] of turtles  
set median-ec      median [eigenvector-centrality] of turtles

set mean-pr        mean [page-rank] of turtles  
set median-pr      median [page-rank] of turtles 

set mean-close     mean [closeness-centrality] of turtles     
set median-close   median [closeness-centrality] of turtles  

set mean-tr        mean   [time-rule] of turtles    
set median-tr      median    [time-rule] of turtles    

set mean-changes   mean   [n-changes] of turtles
set median-changes median [n-changes] of turtles  



end






to shuffle-turtles
set shuffled2? true  ;indicate we're shuffling turtles
;shuffle
ask n-of maxi-before-shuffle turtles with [shuffled? = false][
                                              set rule  1
                                              set shuffled? true 
                                              ] 
ask n-of mini-before-shuffle turtles with [shuffled? = false]     [
                                              set rule  2
                                              set shuffled? true 
                                              ] 
ask n-of conf-before-shuffle turtles with [shuffled? = false]     [
                                              set rule  3
                                              set shuffled? true 
                                              ] 
ask n-of anti-before-shuffle turtles with [shuffled? = false]     [
                                              set rule  4
                                              set shuffled? true 
                                              ] 

ask turtles [set shuffled? false]
end

to count-before-shuffle
set maxi-before-shuffle count turtles with [rule = 1]  
set mini-before-shuffle count turtles with [rule = 2]  
set conf-before-shuffle count turtles with [rule = 3]  
set anti-before-shuffle count turtles with [rule = 4]   
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Export ;;;;;;;;;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;create files 


to create-new-file [prefix]
set file.name word (word prefix FileName ) ".csv"
;set-current-directory ""
end


to print-data-in-file
;; write the information to the file
let spacer ","
file-open file.name
file-print  (list  FileName spacer inicoop  spacer  strength-of-dilemma  spacer  cooperation-rate  spacer  satisfaction-rate  spacer  satisfaction-rate2  spacer
                      maxi  spacer     mini  spacer  conf  spacer    anti  spacer 
                      sat-maxi  spacer   sat-mini  spacer   sat-conf  spacer  sat-anti  spacer  
                      sd-sat-maxi  spacer  sd-sat-mini  spacer  sd-sat-conf  spacer    sd-sat-anti  spacer
                      c-maxi  spacer     c-mini  spacer     c-conf  spacer     c-anti  spacer   
                      cg-maxi  spacer     cg-mini  spacer     cg-conf  spacer  cg-anti  spacer 
                      mean-degree-1  spacer   mean-degree-2  spacer   mean-degree-3  spacer  mean-degree-4  spacer 
                       sd-degree-1  spacer   sd-degree-2  spacer  sd-degree-3  spacer   sd-degree-4  spacer 
  mean-cc-1  spacer   mean-cc-2  spacer   mean-cc-3  spacer   mean-cc-4  spacer 
  sd-cc-1  spacer   sd-cc-2  spacer   sd-cc-3  spacer   sd-cc-4  spacer 
  mean-bc-1  spacer   mean-bc-2  spacer   mean-bc-3  spacer   mean-bc-4  spacer  
  sd-bc-1  spacer   sd-bc-2  spacer   sd-bc-3  spacer   sd-bc-4  spacer  
  mean-ec-1  spacer   mean-ec-2  spacer   mean-ec-3  spacer   mean-ec-4  spacer  
  sd-ec-1  spacer   sd-ec-2  spacer   sd-ec-3  spacer   sd-ec-4  spacer  
  mean-pr-1  spacer   mean-pr-2  spacer   mean-pr-3  spacer   mean-pr-4  spacer 
  sd-pr-1  spacer   sd-pr-2  spacer   sd-pr-3  spacer   sd-pr-4  spacer 
  mean-close-1  spacer   mean-close-2  spacer   mean-close-3  spacer   mean-close-4  spacer  
  sd-close-1  spacer   sd-close-2  spacer   sd-close-3  spacer   sd-close-4  spacer
  max-tr-1  spacer   max-tr-2  spacer   max-tr-3  spacer   max-tr-4   spacer
  min-tr-1 spacer  min-tr-2 spacer  min-tr-3 spacer  min-tr-4   spacer
mean-changes-1 spacer  mean-changes-2 spacer  mean-changes-3 spacer  mean-changes-4 spacer  
sd-changes-1 spacer  sd-changes-2 spacer  sd-changes-3 spacer  sd-changes-4 spacer ticks spacer Rewiring-Probability spacer 
max-cc-1 spacer  max-bc-1 spacer  max-ec-1 spacer  max-pr-1 spacer  
max-cc-2 spacer  max-bc-2 spacer  max-ec-2 spacer  max-pr-2 spacer
max-cc-3 spacer  max-bc-3 spacer   max-ec-3 spacer  max-pr-3 spacer
max-cc-4 spacer  max-bc-4 spacer   max-ec-4 spacer  max-pr-4 spacer
max-close-1 spacer max-close-2 spacer max-close-3 spacer max-close-4 spacer 
mean-degree spacer median-degree  spacer
mean-cc spacer median-cc spacer
mean-bc spacer median-bc spacer
mean-ec spacer median-ec spacer
mean-pr spacer median-pr spacer
mean-close spacer median-close spacer
mean-tr spacer median-tr spacer
mean-changes spacer median-changes spacer
shuffled2? spacer repetitions spacer
Initial-Neighbours spacer Num-Agents spacer diameter spacer network-density spacer average-path-length spacer clustering-coefficient spacer clustering-coefficient-2 spacer n-links 
spacer equivalent-clustering-coefficient spacer equivalent-clustering-coefficient-2 spacer equivalent-path-length spacer lambdaSW spacer gammaSW spacer Sdelta spacer SWtest spacer omega)
file-close
end


to export-data
;;set the directory where the file will be stored
create-new-file ""
set-final-outputs
print-data-in-file

count-before-shuffle
shuffle-turtles
set-final-outputs
print-data-in-file
end

to export-network
set-final-outputs
ask turtles [set neighbors-who  [who] of  turtle-set (sort-on [who] link-neighbors) ]
ask turtles [set neighbors-type [rule] of turtle-set (sort-on [who] link-neighbors) ]

;;set the directory where the file will be stored
create-new-file "Network"

let spacer ","
foreach sort turtles 
[
ask ? [
      file-open file.name
      file-print (list  who spacer cooperate?    spacer rule spacer satisfaction2 spacer
                        node-clustering-coefficient spacer betweenness-centrality spacer
                        eigenvector-centrality  spacer page-rank spacer closeness-centrality spacer
                        longest-path spacer mean-path spacer time-rule spacer time-behavior spacer
                        n-changes spacer strength-of-dilemma spacer inicoop spacer Rewiring-Probability spacer 
                        Num-Agents spacer Initial-Neighbours spacer FileName spacer 
                        clustering-coefficient spacer average-path-length spacer degree spacer n-links spacer 
                        neighbors-type spacer neighbors-who)
      file-close
      ]
]
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Outputs and Plots ;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to my-update-plots  
  set-current-plot "Cooperation"
  set-current-plot-pen "cooperation rate"
  plot cooperation-rate
  set-current-plot-pen "satisfaction"
  plot satisfaction-rate
  set-current-plot-pen "satisfaction2"
  plot satisfaction-rate2
  set-current-plot-pen "Happy and Cooperating"
  plot count turtles with [shape = "face happy" and cooperate?] / Num-Agents
  
  set-current-plot "population"
  set-current-plot-pen "Maxi"
  plot maxi / Num-Agents
  set-current-plot-pen "mini"
  plot mini / Num-Agents
  set-current-plot-pen "Conf"
  plot conf / Num-Agents
  set-current-plot-pen "Anti"
  plot anti / Num-Agents
  

 
  set-current-plot "Degrees Plot"
  set-current-plot-pen "maxi"
  ifelse maxi > 0 [plot mean [degree] of turtles with [rule = 1]][plot 0]
  set-current-plot-pen "mini"
  ifelse mini > 0 [plot mean [degree] of turtles with [rule = 2]][plot 0]
  set-current-plot-pen "conf"
  ifelse conf > 0 [plot mean [degree] of turtles with [rule = 3]][plot 0]
  set-current-plot-pen "anti"
  ifelse anti > 0 [plot mean [degree] of turtles with [rule = 4]][plot 0]
  
  
  set-current-plot "Clustering Coefficient Plot"
  set-current-plot-pen "maxi"
  ifelse maxi > 0 [plot mean [node-clustering-coefficient] of turtles with [rule = 1]][plot 0]
  set-current-plot-pen "mini"
  ifelse mini > 0 [plot mean [node-clustering-coefficient] of turtles with [rule = 2]][plot 0]
  set-current-plot-pen "conf"
  ifelse conf > 0 [plot mean [node-clustering-coefficient] of turtles with [rule = 3]][plot 0]
  set-current-plot-pen "anti"
  ifelse anti > 0 [plot mean [node-clustering-coefficient] of turtles with [rule = 4]][plot 0]
  
  set-current-plot "Page Rank Plot"
  set-current-plot-pen "maxi"
  ifelse maxi > 0 [plot mean [page-rank] of turtles with [rule = 1]][plot 0]
  set-current-plot-pen "mini"
  ifelse mini > 0 [plot mean [page-rank] of turtles with [rule = 2]][plot 0]
  set-current-plot-pen "conf"
  ifelse conf > 0 [plot mean [page-rank] of turtles with [rule = 3]][plot 0]
  set-current-plot-pen "anti"
  ifelse anti > 0 [plot mean [page-rank] of turtles with [rule = 4]][plot 0]
 

   
  end







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
clear-all
;always need to be setup
set Num-Agents *-Num-Agents
set Topology *-Topology
set Strength-of-Dilemma *-strength-of-dilemma
set inicoop *-inicoop
set replacement? *-replacement?

set infinity Num-Agents * 100
set average-path-length infinity
set n-changes-list  []
set time-rule-list  []
set rule-at-death-list  []
set age-at-death-list []
set time-behavior-list  []
set nodes-list []
;only setup if RN
if Topology = "Random" [set Connection-Probability *-Connection-Probability]

;only setup if SW
if Topology = "Small-World" [
                               set Initial-Neighbours *-Initial-Neighbours
                               set Rewiring-Probability *-Rewiring-Probability
                               ]

if Topology = "Scale-Free" [set Scale-Free-Exponent *-Scale-Free-Exponent]
     
 
set Initial-Random-Types? *-Initial-Random-Types?
ifelse not Initial-Random-Types?
      [
      set Initial-Maxi-% *-Initial-Maxi-%
      set Initial-Mini-% *-Initial-Mini-%
      set Initial-Conf-% *-Initial-Conf-%
      set Initial-Anti-% (100 - Initial-Conf-% - Initial-Mini-% - Initial-Maxi-%)
      ]
      [
      set Initial-Maxi-% (random-float 1) * 100
      set Initial-Mini-% (random-float (1 - Initial-Maxi-%)) * 100 
      set Initial-Conf-% (random-float (1 - Initial-Maxi-% - Initial-Mini-%)) * 100
      set Initial-Anti-% (100 - Initial-Conf-% - Initial-Mini-% - Initial-Maxi-%)
      ]

;set Transcription-error 1
;set PER *-p-Error-Copy-Rule
;set PEB *-p-Error-Copy-Behavior

      
;common-setup
;to common-setup

set success? false

if Num-Agents > 500
[
let rows  (ceiling ( sqrt Num-Agents ) ) 
let columns (ceiling (Num-Agents / rows )) 
set rows (rows / 2 ) 
set columns (columns / 2 ) 
resize-world ((-1) * rows) (rows - 1) ((-1) * columns) (columns - 1 )
]
set radius ( ( min (list world-width world-height) ) / 2 - 1)  
;show count patches 



ifelse not load-topology? [setup-Topology] 
[nw:load-matrix FileName turtles links]

;[nw:load-graphml FileName 
; nw:set-context turtles links
; ]



set Num-Agents count turtles

setup-init-turtles

set-life-distribution-USA2010
if replacement? [
                 init-age-USA2010
                ]

set average-path-length nw:mean-path-length

set diameter max [longest-path] of turtles  
ask links [set color gray]
set Gray-links links with [color = gray]


set clustering-coefficient mean  [ node-clustering-coefficient ] of turtles
set clustering-coefficient-2 global-clustering-coefficient
set network-density count links * 2 / ( (Num-Agents - 1) * (Num-Agents))
set shuffled2? false
set n-links count links 
set repetitions 0

set mincc min [node-clustering-coefficient] of turtles
set mindeg min [degree] of turtles
set original-degrees [degree] of turtles

ask turtles [establish-color]
set-outputs
my-update-plots

reset-ticks

end





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Turtles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-init-turtles

if-else Initial-Random-Types? [ask turtles [set rule (random 4) + 1]]      
  [
   ask n-of (floor (Initial-Maxi-% * Num-Agents  / 100 )) turtles [set rule 1]
   ask n-of floor ((Initial-Mini-% * Num-Agents / 100 )) turtles with [rule != 1] [set rule 2]
   ask n-of floor ((Initial-Conf-% * Num-Agents / 100 )) turtles with [rule != 1 and rule != 2] [set rule 3]
   ask turtles with [rule != 1 and rule != 2 and rule != 3] [set rule 4]
  ]
 
ask turtles [      
     set shape "face sad"
     set size 1
     set age 0
     set satisfaction2 1
     ifelse random-float 1.0 < (inicoop / 100)
        [set cooperate? true]
        [set cooperate? false]
     establish-color
     set score 0.0
     set rule? false
     set behavior? false

set betweenness-centrality nw:betweenness-centrality
set eigenvector-centrality nw:eigenvector-centrality
set page-rank nw:page-rank
set closeness-centrality nw:closeness-centrality
set degree count link-neighbors
set node-clustering-coefficient nw:clustering-coefficient 
set time-rule 0
set n-changes 0
set shuffled? false
set distance-from-other-turtles map [nw:distance-to ?] sort turtles      
set longest-path max distance-from-other-turtles

set mean-path mean distance-from-other-turtles

     ;set move? false
;      set copy-error-rule     PER  
;      set copy-error-behavior PEB
;       
;ifelse Asynchronous-Updating?
;     [
;     ifelse random-init-u?
;       [
;       set theta_1 random-float 1.0
;       set theta_2 random-float 1.0
;       ]
;       [
;       set theta_1 Initial-prob-update-behavior
;       set theta_2 Initial-prob-update-rule
;       ]
;     ]
;     [
;     set theta_1 1
;     set theta_2 1
;;     ]

]

end

to establish-color  ;; agent procedure

if-else Colormap-View = "Strategies"
[  if-else rule = 1        [set color red]
    [if-else rule = 2      [set color green]
      [if-else rule = 3    [set color blue]
                           [set color white]]]
]
[
  if-else cooperate? [set color blue] [set color orange]
]
end
@#$#@#$#@
GRAPHICS-WINDOW
25
12
359
353
-1
-1
14.1
1
10
1
1
1
0
0
0
1
-12
10
-11
10
1
1
1
ticks
30.0

BUTTON
161
371
242
404
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
256
371
333
404
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
25
491
163
577
*Strategies colormap\n\nRed        Maxi\nGreen     Mini\nBlue     Conformist\nWhite      Anti-conf\n                      \n                       
9
0.0
0

SLIDER
399
44
580
77
*-strength-of-dilemma
*-strength-of-dilemma
0
0.5
0.5
0.01
1
NIL
HORIZONTAL

PLOT
806
10
1114
130
Cooperation
time
NIL
0.0
500.0
0.0
1.0
true
true
"" ""
PENS
"cooperation rate" 1.0 0 -2674135 true "" ""
"satisfaction" 1.0 0 -13345367 true "" ""
"satisfaction2" 1.0 0 -10022847 true "" ""
"Happy and Cooperating" 1.0 0 -8990512 true "" ""

PLOT
807
132
1114
258
population
time
fraction
0.0
500.0
0.0
1.0
true
true
"" ""
PENS
"Maxi" 1.0 0 -2674135 true "" ""
"mini" 1.0 0 -13840069 true "" ""
"Conf" 1.0 0 -13345367 true "" ""
"Anti" 1.0 0 -16777216 true "" ""

SLIDER
400
80
575
113
*-inicoop
*-inicoop
0
100
50
1
1
NIL
HORIZONTAL

SLIDER
385
313
586
346
*-Connection-Probability
*-Connection-Probability
0.0
1
0.925
.001
1
NIL
HORIZONTAL

INPUTBOX
496
233
572
293
*-Num-Agents
50
1
0
Number

TEXTBOX
386
207
560
225
*Choose Topology\n
10
15.0
1

CHOOSER
400
234
492
279
*-Topology
*-Topology
"Random" "Small-World" "Scale-Free" "Lattice"
0

TEXTBOX
388
301
598
319
Random Network Connection Probability
9
0.0
1

TEXTBOX
389
351
539
369
Small World Parameters
9
0.0
1

SLIDER
386
398
584
431
*-Rewiring-Probability
*-Rewiring-Probability
0
1
1
.001
1
NIL
HORIZONTAL

SLIDER
386
446
583
479
*-Scale-Free-Exponent
*-Scale-Free-Exponent
1.5
3.1
1.83
.01
1
NIL
HORIZONTAL

TEXTBOX
391
434
541
452
Scale-Free Exponent
9
0.0
1

TEXTBOX
147
494
255
570
*Behaviour Colormap\n\nBlue   Cooperate\nOrange Defect
9
0.0
1

TEXTBOX
380
28
555
46
*Prisoner's Dilemma Parameters
9
15.0
1

SLIDER
387
362
584
395
*-Initial-Neighbours
*-Initial-Neighbours
2
*-Num-Agents - 1
6
2
1
NIL
HORIZONTAL

CHOOSER
30
372
145
417
Colormap-View
Colormap-View
"Strategies" "Behaviours"
0

MONITOR
683
153
741
198
Maxi %
count turtles with [ rule = 1 ] * 100 / count turtles
2
1
11

MONITOR
744
154
802
199
Mini %
count turtles with [rule = 2 ] * 100 / count turtles
2
1
11

MONITOR
683
202
743
247
Conf %
count turtles with [rule = 3 ]  * 100 / count turtles
2
1
11

MONITOR
744
202
803
248
Anti %
count turtles with [rule = 4 ] * 100 / count turtles
2
1
11

MONITOR
23
435
86
480
Mean Path Length
average-path-length
3
1
11

MONITOR
89
436
184
481
Clustering Coefficient
clustering-coefficient
3
1
11

TEXTBOX
47
419
197
437
*Network Properties
9
0.0
1

TEXTBOX
385
128
594
152
*Add noise by replacing the population?
9
15.0
1

SWITCH
620
325
795
358
*-Initial-Random-Types?
*-Initial-Random-Types?
0
1
-1000

TEXTBOX
622
310
792
329
*Random Assignation of Types?
9
0.0
1

INPUTBOX
619
374
704
434
*-Initial-Maxi-%
0
1
0
Number

INPUTBOX
706
374
789
434
*-Initial-Mini-%
0
1
0
Number

INPUTBOX
620
434
703
494
*-Initial-Conf-%
50
1
0
Number

MONITOR
709
436
793
481
Initial-Anti-%
100 - *-Initial-Maxi-% - *-Initial-Mini-% - *-Initial-Conf-%
0
1
11

TEXTBOX
620
360
807
380
Otherwise Input % Initial Types
9
0.0
1

TEXTBOX
723
135
823
158
Types %
9
0.0
1

TEXTBOX
403
221
570
241
*Network Parameters
9
0.0
1

MONITOR
708
23
805
68
Cooperation %
count turtles with [cooperate? ] * 100 / count turtles
2
1
11

MONITOR
708
73
804
118
Satisfaction %
count turtles with [shape = \"face happy\"] * 100 / count turtles
2
1
11

MONITOR
187
436
267
481
Density Links
count links * 2 / ( (count turtles - 1) * (count turtles))
2
1
11

PLOT
808
382
1116
502
Clustering Coefficient Plot
Coefficient
Count
0.0
0.05
0.0
1.0
true
true
"" ""
PENS
"maxi" 1.0 0 -2674135 true "" ""
"Mini" 1.0 0 -10899396 true "" ""
"Conf" 1.0 0 -13345367 true "" ""
"Anti" 1.0 0 -16710653 true "" ""

SWITCH
387
509
507
542
load-topology?
load-topology?
1
1
-1000

INPUTBOX
510
502
586
562
FileName
gs.txt
1
0
String

MONITOR
272
436
335
481
Diameter
Diameter
0
1
11

TEXTBOX
389
487
592
518
Load a saved Topology? Specify file:
10
0.0
1

PLOT
808
260
1117
380
Degrees Plot
Degree
Count
1.0
10.0
0.0
10.0
true
true
"" ""
PENS
"maxi" 1.0 0 -2674135 true "" ""
"mini" 1.0 0 -10899396 true "" ""
"conf" 1.0 0 -13345367 true "" ""
"anti" 1.0 0 -16777216 true "" ""

PLOT
808
503
1116
623
Page Rank Plot
Page Rank
Count
0.0
100.0
0.0
0.05
true
true
"" ""
PENS
"maxi" 1.0 0 -2674135 true "" ""
"mini" 1.0 0 -10899396 true "" ""
"conf" 1.0 0 -13345367 true "" ""
"anti" 1.0 0 -16777216 true "" ""

BUTTON
278
494
351
527
Layout
layout
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
277
529
356
562
resize-nodes
resize-nodes
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
407
158
558
191
*-replacement?
*-replacement?
1
1
-1000

MONITOR
622
170
680
216
New Turtles
count turtles with [shape = \"target\"] * 100 / count turtles
2
1
11

TEXTBOX
639
293
789
311
*Choose types distribution
9
15.0
1

@#$#@#$#@
## WHAT IS IT?

This is a version of metamimetic games with agents playing the prisoner's dilemma game:

Agents 




                                 Payoff Matrix
                                 -------------
                                    OPPONENT
          BEHAVIORS   Cooperate            Defect
                        -----------------------------
           Cooperate |(1-p, 1-p)            (0, 1)
      YOU            |
           Defect    |(1, 0)                (p, p)
    
            (x, y) = x: your score, y: your partner's score
            Note: higher the score (amount of the benefit), the better.

whit each one of their neighbours in a torus or network   

The agents can have one of 4 valuation functions:
Maxi : The agent tries to maximize the score (payoff)  
mini : The agent tries to minimize the score  
Conformist: The agent tries to behaves as the majority   
Anti-conformist: The agent tries to behave as the minority
   
## HOW TO USE IT

Decide what percentage of patches should cooperate at the initial stage.

Decide the topology structure or load one.

If you are not loading a topology; choose the parameters for the desired topology. Notice the size of the lattice is fixed and you cant change the number of agents.

Also, choose if agents can incur in errors while copying either rules or behaviors, with the corresponding sliders.

Additionaly, choose if agents can have biased perceptions on their own satisfaction with the chooser error_on_satisfaction

Finally choose if agents can do something different than that of what they see in their neighbourhood through the chooser Innovate?

## HOW IT WORKS

At each period: 

Each agent A plays a prisoner's dilemma game pairwise with all of its neighbours. The scores for all the pairwise games played are summed up to become the new payoffs of the agent.  

Each agent looks at the payoffs, decision-making rules and behaviours of other agents in its neighbourhood Gamma_A. 

For any agent A, if according to A's  type (payoffs based or non-materialistic) there is one neighbour B that is more successful than A himself, and if B has a different decision-making rule, then A copies the rule of agent B. In the case of two or more candidates to copy, then A chooses one of the rules at random. 

If according to its new rule of behaviour and its associated utility function, A is still not among the most successful agents in Gamma_A, then A copies the behaviour of the neighbour with the best situation.


As an example:

 If agent A had the conformist type and if the majority of its neighbours have turned to maxi since last round, then A will adopt the maxi rule. Here, the imitation rule is used to update itself (reflexivity).
 If same agent A, which is now a maxi agent, played C last round but a D-player did strictly better than all of A’s neighbours (A included), then A will become a D-player. Here, the imitation rule is used to update the behaviour (metacognition).


Maxi and mini agents want to maximize or minimize their scores and look for those neighbours who did better score-wise. Conformists and anti-conformists desire to be in the majority or the minority respectively.


## THINGS TO NOTICE

Parameters of the network can be observed bellow the world.

How do populations change with the strength of the dilemma?

What are the effects of the different types of noise?

Where are agents located in the network at the attractor? 
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.1.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="RandomNets" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export-data</final>
    <exitCondition>all? turtles [shape = "face happy"] and ticks &gt; 1</exitCondition>
    <enumeratedValueSet variable="*-inicoop">
      <value value="0"/>
      <value value="20"/>
      <value value="60"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-strength-of-dilemma">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.3"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;R015.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="load-topology?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-random-init-u?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Asynchronous-Updating?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-prob-update-behavior">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-prob-update-rule">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Num-Agents">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Transcription-error">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Random-Types?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Copy-Error-Random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Behavior">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Rule">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Topology">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Rewiring-Probability">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Neighbours">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Scale-Free-Exponent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Connection-Probability">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Colormap-View">
      <value value="&quot;Strategies&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Conf-%">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Maxi-%">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Mini-%">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SWNets" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export-data</final>
    <exitCondition>(all? turtles [shape = "face happy"] and ticks &gt; 1) or ticks  &gt; 500</exitCondition>
    <enumeratedValueSet variable="*-inicoop">
      <value value="0"/>
      <value value="20"/>
      <value value="60"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-strength-of-dilemma">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.3"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;SW2005.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="load-topology?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-random-init-u?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Asynchronous-Updating?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-prob-update-behavior">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-prob-update-rule">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Num-Agents">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Transcription-error">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Random-Types?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Copy-Error-Random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Behavior">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Rule">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Topology">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Rewiring-Probability">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Neighbours">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Scale-Free-Exponent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Connection-Probability">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Colormap-View">
      <value value="&quot;Strategies&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Conf-%">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Maxi-%">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Mini-%">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SFNets" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export-data</final>
    <exitCondition>(all? turtles [shape = "face happy"] and ticks &gt; 1) or ticks  &gt; 500</exitCondition>
    <enumeratedValueSet variable="*-inicoop">
      <value value="0"/>
      <value value="20"/>
      <value value="60"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-strength-of-dilemma">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.3"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;SF21.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="load-topology?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-random-init-u?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Asynchronous-Updating?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-prob-update-behavior">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-prob-update-rule">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Num-Agents">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Transcription-error">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Random-Types?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Copy-Error-Random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Behavior">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Rule">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Topology">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Rewiring-Probability">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Neighbours">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Scale-Free-Exponent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Connection-Probability">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Colormap-View">
      <value value="&quot;Strategies&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Conf-%">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Maxi-%">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Mini-%">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="RandomNets?issing" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export-data</final>
    <exitCondition>(all? turtles [shape = "face happy"] and ticks &gt; 1) or ticks  &gt; 500</exitCondition>
    <enumeratedValueSet variable="*-inicoop">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-strength-of-dilemma">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;R015.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="load-topology?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-random-init-u?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Asynchronous-Updating?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-prob-update-behavior">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-prob-update-rule">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Num-Agents">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Transcription-error">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Random-Types?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Copy-Error-Random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Behavior">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Rule">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Topology">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Rewiring-Probability">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Neighbours">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Scale-Free-Exponent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Connection-Probability">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Colormap-View">
      <value value="&quot;Strategies&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Conf-%">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Maxi-%">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Mini-%">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SWFinalFEB" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export-data
export-network-data</final>
    <exitCondition>(all? turtles [shape = "face happy"] and ticks &gt; 1) or ticks  &gt; 100</exitCondition>
    <enumeratedValueSet variable="Colormap-View">
      <value value="&quot;Strategies&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="*-strength-of-dilemma" first="0" step="0.1" last="0.5"/>
    <enumeratedValueSet variable="*-inicoop">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Random-Types?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Num-Agents">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="error_on_satisfaction">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Innovate?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Behavior">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Rule">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Copy-Thetas?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="load-topology?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Neighbours">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;FEB15&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Topology">
      <value value="&quot;Small-World&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="*-Rewiring-Probability" first="0.025" step="0.025" last="0.975"/>
  </experiment>
  <experiment name="Lattice" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>(all? turtles [shape = "face happy"] and ticks &gt; 1) or ticks  &gt; 200</exitCondition>
    <metric>count turtles with [rule = 1]</metric>
    <metric>count turtles with [rule = 2]</metric>
    <metric>count turtles with [rule = 3]</metric>
    <metric>count turtles with [rule = 4]</metric>
    <enumeratedValueSet variable="*-Initial-Random-Types?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Topology">
      <value value="&quot;Lattice&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Rule">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Innovate?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Behavior">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Colormap-View">
      <value value="&quot;Strategies&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Copy-Thetas?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="error_on_satisfaction">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="load-topology?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;lattice.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-strength-of-dilemma">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-inicoop">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Lattice2" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>(all? turtles [shape = "face happy"] and ticks &gt; 1) or ticks  &gt; 300</exitCondition>
    <metric>count turtles with [rule = 1]</metric>
    <metric>count turtles with [rule = 2]</metric>
    <metric>count turtles with [rule = 3]</metric>
    <metric>count turtles with [rule = 4]</metric>
    <enumeratedValueSet variable="*-Initial-Random-Types?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Topology">
      <value value="&quot;Lattice&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Rule">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Innovate?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Behavior">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Colormap-View">
      <value value="&quot;Strategies&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Copy-Thetas?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="error_on_satisfaction">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="load-topology?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;lattice.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-strength-of-dilemma">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-inicoop">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Lattice3" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>(all? turtles [shape = "face happy"] and ticks &gt; 1) or ticks  &gt; 300</exitCondition>
    <metric>count turtles with [rule = 1] / count turtles</metric>
    <metric>count turtles with [rule = 2] / count turtles</metric>
    <metric>count turtles with [rule = 3] / count turtles</metric>
    <metric>count turtles with [rule = 4] / count turtles</metric>
    <enumeratedValueSet variable="*-Initial-Random-Types?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Topology">
      <value value="&quot;Lattice&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Rule">
      <value value="0.02"/>
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Innovate?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Behavior">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Colormap-View">
      <value value="&quot;Strategies&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Copy-Thetas?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="error_on_satisfaction">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="load-topology?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;lattice3.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-strength-of-dilemma">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-inicoop">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export-data
export-network</final>
    <exitCondition>(all? turtles [shape = "face happy"] and ticks &gt; 1) or ticks  &gt; 100</exitCondition>
    <enumeratedValueSet variable="Colormap-View">
      <value value="&quot;Strategies&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-strength-of-dilemma">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-inicoop">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Random-Types?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Num-Agents">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="error_on_satisfaction">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Innovate?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Behavior">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Rule">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Copy-Thetas?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="load-topology?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Neighbours">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;experimento&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Topology">
      <value value="&quot;Small-World&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Rewiring-Probability">
      <value value="0.3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SWFinalRun" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export-data
export-network</final>
    <exitCondition>(all? turtles [shape = "face happy"] and ticks &gt; 1) or ticks  &gt; 100</exitCondition>
    <enumeratedValueSet variable="Colormap-View">
      <value value="&quot;Strategies&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="*-strength-of-dilemma" first="0" step="0.1" last="0.5"/>
    <enumeratedValueSet variable="*-inicoop">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Random-Types?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Num-Agents">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="error_on_satisfaction">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Innovate?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Behavior">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Rule">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Copy-Thetas?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="load-topology?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Neighbours">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;FinalFeb15&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Topology">
      <value value="&quot;Small-World&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="*-Rewiring-Probability" first="0" step="0.025" last="1"/>
  </experiment>
  <experiment name="SWSuffling" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export-network</final>
    <exitCondition>(all? turtles [shape = "face happy"] and ticks &gt; 1) or ticks  &gt; 100</exitCondition>
    <enumeratedValueSet variable="Colormap-View">
      <value value="&quot;Strategies&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-strength-of-dilemma">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-inicoop">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Random-Types?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Num-Agents">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="error_on_satisfaction">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Innovate?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Behavior">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Rule">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Copy-Thetas?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="load-topology?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Neighbours">
      <value value="4"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;Shuffling&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Topology">
      <value value="&quot;Small-World&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="*-Rewiring-Probability" first="0" step="0.05" last="0.5"/>
  </experiment>
  <experiment name="SWShufflingExperiment" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export-network</final>
    <exitCondition>(all? turtles [shape = "face happy"] and ticks &gt; 1) or ticks  &gt; 100</exitCondition>
    <enumeratedValueSet variable="Colormap-View">
      <value value="&quot;Strategies&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-strength-of-dilemma">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-inicoop">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Random-Types?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Num-Agents">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="error_on_satisfaction">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Innovate?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Behavior">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Rule">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Copy-Thetas?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="load-topology?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Neighbours">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;experimento&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Topology">
      <value value="&quot;Small-World&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Rewiring-Probability">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SWShuffling2" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export-network</final>
    <exitCondition>(all? turtles [shape = "face happy"] and ticks &gt; 1) or ticks  &gt; 100</exitCondition>
    <enumeratedValueSet variable="Colormap-View">
      <value value="&quot;Strategies&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-strength-of-dilemma">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-inicoop">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Random-Types?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Num-Agents">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="error_on_satisfaction">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Innovate?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Behavior">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Rule">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Copy-Thetas?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="load-topology?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Neighbours">
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;Shuffling2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Topology">
      <value value="&quot;Small-World&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="*-Rewiring-Probability" first="0" step="0.01" last="0.2"/>
  </experiment>
  <experiment name="LatticeMinis" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>(all? turtles [shape = "face happy"] and ticks &gt; 1) or ticks  &gt; 100</exitCondition>
    <metric>count turtles with [rule = 1] / count turtles</metric>
    <metric>count turtles with [rule = 2] / count turtles</metric>
    <metric>count turtles with [rule = 3] / count turtles</metric>
    <metric>count turtles with [rule = 4] / count turtles</metric>
    <enumeratedValueSet variable="*-Initial-Random-Types?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Num-Agents">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Topology">
      <value value="&quot;Lattice&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Rule">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Innovate?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Behavior">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Colormap-View">
      <value value="&quot;Strategies&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Copy-Thetas?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="error_on_satisfaction">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="load-topology?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;latticeMinis.txt&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="*-strength-of-dilemma" first="0" step="0.01" last="0.5"/>
    <enumeratedValueSet variable="*-inicoop">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="LatticeMinisCoop" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>(all? turtles [shape = "face happy"] and ticks &gt; 1) or ticks  &gt; 100</exitCondition>
    <metric>count turtles with [rule = 1] / count turtles</metric>
    <metric>count turtles with [rule = 2] / count turtles</metric>
    <metric>count turtles with [rule = 3] / count turtles</metric>
    <metric>count turtles with [rule = 4] / count turtles</metric>
    <metric>count turtles with [cooperate? = TRUE] / count turtles</metric>
    <metric>count turtles with [rule = 1 and cooperate? = TRUE] / count turtles with [rule = 1]</metric>
    <metric>count turtles with [rule = 2 and cooperate? = TRUE] / count turtles with [rule = 2]</metric>
    <metric>count turtles with [rule = 3 and cooperate? = TRUE] / count turtles with [rule = 3]</metric>
    <metric>count turtles with [rule = 4 and cooperate? = TRUE] / count turtles with [rule = 4]</metric>
    <enumeratedValueSet variable="*-Initial-Random-Types?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Num-Agents">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Topology">
      <value value="&quot;Lattice&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Rule">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Innovate?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-p-Error-Copy-Behavior">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Colormap-View">
      <value value="&quot;Strategies&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Copy-Thetas?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="error_on_satisfaction">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="load-topology?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FileName">
      <value value="&quot;latticeMinisCop.txt&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="*-strength-of-dilemma" first="0" step="0.1" last="0.5"/>
    <steppedValueSet variable="*-inicoop" first="0" step="15" last="100"/>
  </experiment>
</experiments>
@#$#@#$#@
VIEW
55
40
385
370
0
0
0
1
1
1
1
1
0
1
1
1
-12
10
-11
10

@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
