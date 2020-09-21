#|
  YAMSS - Yet Another Mini Sokoban Solver
  Author    : Brian Damgaard, Denmark
  E-mail    : BrianDamgaard@jubii.dk
  Copyright : (c) 2014 by Brian Damgaard, Denmark 
  Version   : 0.1
  License   : GPL
  ______________________________________________________________________________

  YAMSS - Yet Another Mini Sokoban Solver
  Copyright (c) 2014 Brian Damgaard, Denmark

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.
 
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
  ______________________________________________________________________________

  Preconditions

    * A character set based on ASCII or Unicode, with the ordinal values of 
      control characters like "carriage return" and "line feed" less than 
      the ordinal value of #\space.
  ______________________________________________________________________________

  Usage

    In a Common Lisp implementation with both an interpreter and a compiler, the
    solver must be compiled before it can be used, otherwise the solver is too
    slow. For instance, in CLisp from "http://clisp.org/" (retrieved 2014-08-13) 
    the procedure looks like this:
      (compile-file "yamss.lsp") ; add the appropriate path name prefix
      (load         "yamss.fas") ; add the appropriate path name prefix     

    To run the solver, call the function "solve" in the "Sokoban" package with
    the following parameters:
      (solve  
       :input    input-file-name                 ; level file, "SOK" format
       :output   output-file-name                ; level file, "SOK" format
       :strategy search-strategy                 ; backward, forward, perimeter
       :table    transposition-table-size-limit  ; number of stored positions
       :time     search-limit-time-seconds       ; seconds
       :start    level-start-index               ; 1-based, inclusive
       :end      level-end-index)                ; 1-based, inclusive

    Example:
      (in-package "Sokoban")  ; change package, i.e., namespace, to "Sokoban"
      (solve :input "levels.sok" :output "solutions.sok" 
             :time 60 :start 1 :end 10)
    This call attempts to solve the first ten levels in the specified file with
    a 60 seconds time limit for each level. 

    All parameter values default to their last entered values, so the next ten
    levels in the file can now be processed with the same time limit this way:
      (solve :start 11 :end 20)
  ______________________________________________________________________________

  Highlights in no Particular Order

    * Even though this is a "mini solver program", it has highly efficient data 
      structures and many sophisticated general algorithms as well as many
      Sokoban specific algorithms.

    * Search Strategies:
        * Perimeter search (default strategy)
          The perimeter search consists of first a backward search, and then a
          forward search toward the retained frontier positions from the 
          backward search, recycling the retained frontier positions gradually 
          if memory runs full before the forward search finds a solution. If the
          forward search finds a solution, then a second backward search is 
          launched to reconstruct the path from the frontier position to the 
          goal position. This second backward search fails if the total search 
          time exceeds the user-defined time limit per level.
        * Backward search
        * Forward search

    * Search algorithm: A* with depth-first extensions and with node
      recycling of the least promising positions when memory runs full. 
      It's not a full SMA* algorithm though. (SMA*, "simplified memory 
      bounded A* search, has the same optimality guarantee as A*.)

    * A Sokoban solver spends a lot of time calculating the pusher's 
      reachable squares for each position in the game. To that end, the 
      calculations are performed by means of timestamps in recycled areas, 
      thereby avoiding a costly "initialize-to-zero" step, which otherwise 
      would have been required for each calculation.

    * The internal representation of the game board has a wall-filled border 
      around the original board loaded from the external source. This makes
      it easier and more efficient to calculate legal pusher moves and box 
      moves because "falling off the board" cannot happen, even if the 
      original board doesn't have a wall-filled border itself.

    * Position identity checking is based on Zobrist hash keys which can
      be updated incrementally when boxes move around on the board. This is
      more efficient than recalculating a hash value from scratch for each 
      position.

    * Level Preprocessing
      * "Fill tubes" fills all dead end floor squares with walls, also 
        when this involves making forced initial pusher moves and box pushes
        on the board. The idea and the outline of the method used here for 
        the calculation should be attributed to Lee J Haywood.

      * For each floor square, the distances to the nearest goal position
        and the nearest box starting position are calculated. Based on this 
        information, unreachable squares and squares from which a goal 
        cannot be reached are flagged as "dead squares" directly on the 
        board. During the search, these flags block the way for the boxes 
        exactly like walls, with little or no extra run time costs, and is 
        thus an inexpensive first line of defense against deadlock moves.

      * "Gate squares" are floor squares on the board which, when occupied
        by a box, split the board in two or more unconnected areas for the
        pusher. Knowing which squares are gate squares is often a 
        prerequisite for further board topology analysis. For instance, it 
        helps making the tunnel square calculation more accurate (see 
        below).

      * "Tunnel squares" are floor squares where a box can and should 
        continue its movement in the current direction without interleaving 
        other box movements. Put differently, tunnel squares are "slippery" 
        and a box never rests on a tunnel square. During the search, this
        reduces the branching factor. Whether a floor square acts as a tunnel
        square depends on the move direction, and it also depends on the
        search direction, i.e., it's different for a forward search and a
        backward search. The idea and the outline of the method used here for 
        the calculation should be attributed to Matthias Meger.

    * Deadlock Detection
      Two basic deadlock detection mechanisms are implemented.

      * "Dead Squares"
        There are two types of dead squares:

        * Squares from which a box cannot be pushed to a goal square, even when
          there are no other boxes on the board to get in the way. A forward
          search must not push a box to one of these squares.

        * Squares which cannot be reached by pushing a box from one of the
          box starting positions, even when there are no other boxes on the
          board to get in the way. A backward search must not pull a box to one
          of these squares.

        These dead squares are flagged directly on the board and block the way 
        for the boxes exactly like walls, with little or no extra run time costs 
        during the search.
         
      * "Freeze Deadlocks"   
        A "freeze deadlock" occurs when a box move causes one or more boxes to
        become immovable, and at least one of these immovable boxes isn't 
        located at a goal square. All "freeze deadlocks" with just one box are
        also detected by the "dead squares" mechanism.

        "Freeze deadlock" box configurations are very different for a forward
        search and a backward search, and they require different algorithms.
        Both algorithms handle box clusters of arbitrary size and complexity,
        and both algorithms require memoization to dampen exponential growth.
      
    * The open queue contains the positions to be expanded, i.e., to have 
      their successor positions generated and examined. The queue items must 
      be sorted in ascending order on position scores, so the most promising
      positions easily can be selected for expansion before the less promising
      positions. This sorting is implemented with a so-called "bucket sort", 
      meaning that the score is simply used as an index into a vector, where 
      each vector element contains a linked list with all positions with that 
      score. This method is as efficient as sorting possibly can be, at the
      expense of putting a limit on the range of acceptable score values.

    * Levels with trivial zero-pushes solutions are handled correctly. First,
      a search for a non-trivial solution is performed. If it is unsuccessful,
      then the level is reported as solved with a trivial zero-pushes solution.

    * After use, objects like transposition table items and timestamp boards 
      are kept on free lists and recycled by the solver rather than releasing 
      them to garbage collection by the underlying Lisp system. The hope is that 
      this is more efficient than always allocating fresh objects. At least,
      it makes it a little easier to rewrite the solver in languages without 
      automatic garbage collection.

    * In Sokoban, the squares on the board have four sides, but the solver is
      coded so changing the relevant constants makes the program work for any 
      variant of the game with an even number of board cell sides.
  ______________________________________________________________________________
|#

(defpackage "Sokoban")
(in-package "Sokoban")

(eval-when (:execute :load-toplevel :compile-toplevel compile load eval)

(defmacro      mc        (&rest r) `(defmacro ,@r))
(mc            fn        (&rest r) `(defun    ,@r))
(mc            do..      (&rest r) `(progn    ,@r))
(mc            get-tick-count
                         () `(get-internal-real-time))
(mc            inc!      (&rest r) `(incf  ,@r))
(mc            new-symbol
                         (&rest r) `(gensym   ,@r))
(mc            set!      (&rest r) `(setf     ,@r))

(mc            append!   (&rest r) `(nconc    ,@r))
(mc            bit?      (index value) `(logbitp ,index ,value))
(mc            bitand    (&rest r) `(logand   ,@r))
(mc            bitandc1  (&rest r) `(logandc1 ,@r))
(mc            bitor     (&rest r) `(logior   ,@r))
(mc            bit-shift-arithmetic
                         (&rest r) `(ash      ,@r))  
(mc            bitxor    (&rest r) `(logxor   ,@r))
(mc            boolean-to-integer
                         (value)   `(if ,value 1 0))
(mc            boolean-to-yes-no   
                         (value)   `(if ,value TEXT_YES TEXT_NO))
(mc            clear-bit! 
                         (index value)
                         `(setf ,value (bitandc1 (bit-shift-arithmetic 1 ,index) ,value)))
(mc            const     (&rest r) `(defconstant ,@r))
(mc            dec!      (&rest r) `(decf ,@r)) 
(mc            display1  (item) `(princ ,item))
(mc            displayln (item) `(progn (display1 ,item) (nl)))
(mc            display  (&rest r)
                         `(do.. (dolist (item (list ,@r)) (display1 item) (display1 " ")) (nl)))
(mc            tick-count-to-milliseconds
                         (tick-count) `(* 1000 (/ ,tick-count internal-time-units-per-second))) 
(mc            elapsed-time-ticks        ; no wraparound guard
                         (from-tick-count) `(- (get-tick-count) ,from-tick-count))
(fn            elapsed-time-milliseconds ; no wraparound guard
                         (from-tick-count)
                         (tick-count-to-milliseconds (elapsed-time-ticks from-tick-count)))
(mc            enum      (&rest r) ; returns the number of enumeration constants
    (let ((n 0) constants)
      (dolist (item r)(push `(const ,item (1- ,(inc! n))) constants))
      `(do.. ,@constants ,n)))
(mc            equal?    (&rest r) `(equal ,@r))
(mc            for-each-hash-table-item ; (key value table) &body)
                         (key-value-table &rest body)
                         `(maphash #'(lambda (,(first  key-value-table)
                                              ,(second key-value-table))
                                        ,@body)
                                   ,(third key-value-table)))
(mc            hash-table-add! 
                         (key value table) `(set! (gethash ,key ,table) ,value))
(mc            hash-table-clear!
                         (table) `(clrhash ,table))
(mc            hash-table-lookup
                         (key table) `(gethash ,key ,table))
(mc            hash-table-remove! 
                         (key table) `(remhash ,key ,table))
;(const        inf       (+ most-positive-double-float most-positive-double-float))
;(const       -inf       (- inf))
(fn            iota      (count)  (let (result)
                                    (dotimes (i count result)
                                      (push (- count i 1) result))))
(mc            list?     (value) `(listp ,value))
(mc            list-tail (list position) `(nthcdr ,position ,list))
(mc            local-fns (&rest r) `(labels ,@r)) ; recursively defined local functions
(mc            maximize! (variable value) `(if (<    ,variable ,value) 
                                               (set! ,variable ,value)
                                               ,variable))
(mc            milliseconds-to-tick-count
                         (milliseconds) `(/ (* ,milliseconds internal-time-units-per-second)
                                            1000))
(mc            minimize! (variable value) `(if (>    ,variable ,value) 
                                               (set! ,variable ,value)
                                               ,variable))
;(const        nan       (/ inf -inf))
(mc            not-equal? 
                         (&rest r) `(not (equal? ,@r)))
(mc            nl        (&rest r) `(terpri  ,@r)) ; emits a newline character
(mc            number?   (&rest r) `(numberp ,@r))
(mc            odd?      (n) `(eql 1 (mod ,n 2)))
(mc            pair?     (item) `(consp ,item))
(mc            remainder (&rest r) `(rem     ,@r))
(mc            reverse!  (list) `(nreverse ,list))
(fn            round-up-to-even
                         (value) (if (odd? value) (1+ value) value))
(mc            same-object?
                         (a b) `(eq ,a ,b))
(mc            set-bit!  (index value)
                         `(set! ,value (bitor (bit-shift-arithmetic 1 ,index) ,value)))
(mc            set-car!  (item value) `(rplaca ,item ,value))
(mc            set-cdr!  (item value) `(rplacd ,item ,value))
(mc            sort!     (sequence &optional predicate)
                         `(stable-sort ,sequence (if ,predicate ,predicate #'<))) 
(mc            string?   (&rest r) `(stringp ,@r))
(mc            struct    (&rest r) `(defstruct ,@r))
(mc            var       (&rest r) `(defparameter ,@r))
(mc            while     (expr &rest body)
                         (let ((loop (new-symbol)))
                           `(tagbody ,loop
                              (if ,expr (do.. ,@body (go ,loop))))))
(mc            with-saved-values (variable-names &rest body) 
  ; e.g., (with-saved-values (a b) (do1) (do2)) =>
  ; (let ((#:g1 a) (#:g2 b)) (unwind-protect (do.. (do1) (do2)) (set! b #:g2) (set! a #:g1)))    
  (let ((original-values-variable-names 
         (let ((result nil)) 
           (dotimes (index (length variable-names) result) (push (new-symbol) result)))))
    `(let ,(mapcar #'(lambda (variable value) (list variable value)) original-values-variable-names variable-names)
       (unwind-protect (do.. ,@body)       
                       ,@(reverse! (mapcar #'(lambda (variable value) (list 'set! variable value)) variable-names original-values-variable-names))))))
(mc            zero?     (value) `(zerop ,value))

(fn load-text-lines-from-file (file-name)
  (with-open-file (f file-name :direction :input)
    (let ((result nil)
          line)
      (while (set! line (read-line f nil))
             (push line result))
      (reverse! result))))

(fn save-text-lines-to-file (lines file-name)
  (with-open-file (f file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
    (while lines
      (write-line (pop lines) f))))

; minimal standard random number generator. 
; don't change the constants unless you know what you're doing.
; the algorithm is described in the article:
; Park, Steven K. and Miller, Keith W., "Random Number Generators:
; Good Ones are Hard to Find", 
; Communications of the ACM, October 1988, Volume 31 Number 10.
(const         random32-A    16807)            ; 7^5
(const         random32-M    2147483647)       ; 2^31 - 1, a prime number
(const         random32-Q    127773)           ; quotient ( M / A )
(const         random32-R    2836)             ; remainder( M / A )
(var           random32-seed 1)                ; must be [1..M-1]
(fn random32   (&optional limit) ; returns an integer in the interval [0..2^31-2]
  (if (or (<=  random32-seed 0)
          (>=  random32-seed random32-M))      ; true: seed not properly initialized yet
      (set!    random32-seed 1))               ; start a new period
  (set!        random32-seed (- (* random32-A (remainder random32-seed random32-Q))
                                (* random32-R (truncate  random32-seed random32-Q))))
  (if (<=      random32-seed 0) ; "=" cannot happen, but the check doesn't hurt
      (inc!    random32-seed random32-M))
  (if limit
      (remainder (1- random32-seed) limit)
      (1-      random32-seed))) ; return [0..M-2]. the seed is [1..M-1].

(const         MIN_BOARD_WIDTH                 3)
(const         MIN_BOARD_HEIGHT                3)

(const         DIRECTION_UP                    0) ; directions: 0 .. DIRECTION_COUNT - 1
(const         DIRECTION_LEFT                  1)
(const         DIRECTION_DOWN                  2)
(const         DIRECTION_RIGHT                 3)
(const         DIRECTION_COUNT                 4) ; precondition: even
(const         AXIS_COUNT                      (bit-shift-arithmetic DIRECTION_COUNT -1))
(const         AXIS_MASK                       (1- (bit-shift-arithmetic 1 (1- AXIS_COUNT))))

(const         WALL_BIT                        4)   ; 1   square values after right-shifting "DIRECTION_COUNT" bits
(const         FLOOR_BIT                       5)   ; 2
(const         BOX_BIT                         6)   ; 4
(const         GOAL_BIT                        7)   ; 8
(const         PUSHER_BIT                      8)   ; 16
(const         ILLEGAL_BOX_SQUARE_BIT          9)   ; 32
(const         UNREACHABLE_BOX_SQUARE_BIT      10)  ; 64
(const         GATE_BIT                        11)  ; 128
(const         BOX_START_SQUARE_BIT            12)  ; 256 
(const         VISITED_SQUARE_BIT              13)  ; 512

(const         EMPTY_SQUARE                    0)
(const         TUNNEL_FLAG_UP                  (bit-shift-arithmetic 1 DIRECTION_UP))
(const         TUNNEL_FLAG_LEFT                (bit-shift-arithmetic 1 DIRECTION_LEFT))
(const         TUNNEL_FLAG_DOWN                (bit-shift-arithmetic 1 DIRECTION_DOWN))
(const         TUNNEL_FLAG_RIGHT               (bit-shift-arithmetic 1 DIRECTION_RIGHT))
(const         WALL                            (bit-shift-arithmetic 1 WALL_BIT))
(const         FLOOR_VALUE                     (bit-shift-arithmetic 1 FLOOR_BIT))
(const         BOX                             (bit-shift-arithmetic 1 BOX_BIT))
(const         GOAL                            (bit-shift-arithmetic 1 GOAL_BIT))
(const         PUSHER                          (bit-shift-arithmetic 1 PUSHER_BIT))
(const         ILLEGAL_BOX_SQUARE              (bit-shift-arithmetic 1 ILLEGAL_BOX_SQUARE_BIT))
(const         UNREACHABLE_BOX_SQUARE          (bit-shift-arithmetic 1 UNREACHABLE_BOX_SQUARE_BIT))
(const         GATE_SQUARE                     (bit-shift-arithmetic 1 GATE_BIT))
(const         BOX_START_SQUARE                (bit-shift-arithmetic 1 BOX_START_SQUARE_BIT))
(const         VISITED_SQUARE                  (bit-shift-arithmetic 1 VISITED_SQUARE_BIT))
(const         BOARD_OBJECT_BITS               (+ WALL FLOOR_VALUE BOX GOAL PUSHER))
(const         TUNNEL_FLAG_BITS                (+ TUNNEL_FLAG_UP   TUNNEL_FLAG_LEFT
                                                  TUNNEL_FLAG_DOWN TUNNEL_FLAG_RIGHT))

(const         BOX+FLOOR_VALUE                 (+ BOX FLOOR_VALUE))
(const         BOX+GOAL+FLOOR_VALUE            (+ BOX GOAL FLOOR_VALUE))
(const         GOAL+FLOOR_VALUE                (+ GOAL FLOOR_VALUE))
(const         PUSHER+FLOOR_VALUE              (+ PUSHER FLOOR_VALUE))
(const         PUSHER+GOAL+FLOOR_VALUE         (+ PUSHER GOAL FLOOR_VALUE))
(const         WALL+BOX+ILLEGAL_BOX_SQUARE     (+ WALL BOX ILLEGAL_BOX_SQUARE))
(const         WALL+ILLEGAL_BOX_SQUARE+UNREACHABLE_BOX_SQUARE+FLOOR_VALUE
                                               (+ WALL ILLEGAL_BOX_SQUARE UNREACHABLE_BOX_SQUARE FLOOR_VALUE))
(const         WALL+BOX+ILLEGAL_BOX_SQUARE+UNREACHABLE_BOX_SQUARE+FLOOR_VALUE
                                               (+ BOX WALL+ILLEGAL_BOX_SQUARE+UNREACHABLE_BOX_SQUARE+FLOOR_VALUE))

(const         BOARD_CHAR_BOX                  #\$)
(const         BOARD_CHAR_BOX_ON_GOAL          #\*)
(const         BOARD_CHAR_FLOOR1               #\space)
(const         BOARD_CHAR_FLOOR2               #\-)
(const         BOARD_CHAR_FLOOR3               #\_)
(const         BOARD_CHAR_GOAL                 #\.)
(const         BOARD_CHAR_PUSHER               #\@)
(const         BOARD_CHAR_PUSHER_ON_GOAL       #\+)
(const         BOARD_CHAR_WALL                 #\#)
(const         BOARD_CHAR_UNKNOWN              #\?)
(const         MAX_ASCII                       127)

(const         BOARD_INDEX_PUSHER              0) ; board header fields. the game board has an extra top row with these fields.
(const         BOARD_INDEX_WIDTH               1)
(const         BOARD_INDEX_HEIGHT              2)
(const         BOARD_INDEX_TIMESTAMP           3) ; must be last board header field

(const         DIRECTION_TO_LOWERCASE_CHAR               
  (make-array  DIRECTION_COUNT :element-type 'character :initial-contents '(#\u #\l #\d #\r)))
 
(const         DISTANCE_INFINITY               most-positive-fixnum)
(const         DISPLAY_STATUS_TIME_INTERVAL_SECONDS
                                               5) 
(const         MAX_TIMESTAMP                   (- MOST-POSITIVE-FIXNUM DIRECTION_COUNT 10))
(const         ZOBRIST_BIT_STRING_BIT_SIZE     64)

(const         INFINITY_SCORE                  most-positive-fixnum)
(const         DEADLOCK_SCORE                  (1- INFINITY_SCORE))
(const         DEAD_END_SCORE                  (1- DEADLOCK_SCORE))
(const         UNDEFINED_SCORE                 (1- DEAD_END_SCORE))   
(const         TERMINATE_SEARCH_SCORE          -1) ; must be -1, right below the scores found on the open queue 
(const         INVALID_PATH_SCORE              -2) ; the special score values must match the list "TEXT_SEARCH_STATES"
(const         PUSHES_LIMIT_EXCEEDED_SCORE     -3)
(const         TIME_LIMIT_EXCEEDED_SCORE       -4)
(const         TRANSPOSITION_TABLE_FULL_SCORE  -5)
(const         NOT_SOLVED_SCORE                -6)
(const         SOLVED_SCORE                    -7)
(const         NO_SEARCH_PROGRESS_PUSH_COUNT_BIT_MASK ; must be a 2^n - 1 number, where n is a non-negative integer
                                               7)
(const         POSITION_FLAG_FREE_BIT          0) ; the position is on the free list, i.e., not in the transposition table
(const         POSITION_FLAG_HASH_TABLE_BIT    1) ; the position is in the hash table. other positions with the same hash value may exist.  
(const         POSITION_FLAG_OPEN_BIT          2) ; the position is on the open queue
(const         POSITION_FLAG_PATH_BIT          3) ; the position is on the path to the current position on the board
(const         POSITION_FLAG_PERIMETER_BIT     4) ; the position is a leaf node from a search in the opposite direction
(const         POSITION_FLAG_REVISIT_BIT       5) ; the position must have all its successors generated if the position is expanded again
(const         POSITION_FLAG_SOLUTION_PATH_BIT 6) ; the position is on the best found solution path
(const         POSITION_FLAG_FREE              (bit-shift-arithmetic 1 POSITION_FLAG_FREE_BIT))
(const         POSITION_FLAG_HASH_TABLE        (bit-shift-arithmetic 1 POSITION_FLAG_HASH_TABLE_BIT))
(const         POSITION_FLAG_OPEN              (bit-shift-arithmetic 1 POSITION_FLAG_OPEN_BIT))
(const         POSITION_FLAG_PATH              (bit-shift-arithmetic 1 POSITION_FLAG_PATH_BIT))
(const         POSITION_FLAG_PERIMETER         (bit-shift-arithmetic 1 POSITION_FLAG_PERIMETER_BIT))
(const         POSITION_FLAG_REVISIT           (bit-shift-arithmetic 1 POSITION_FLAG_REVISIT_BIT))
(const         POSITION_FLAG_SOLUTION_PATH     (bit-shift-arithmetic 1 POSITION_FLAG_SOLUTION_PATH_BIT))
(const         POSITION_FLAGS_PATH+SOLUTION_PATH
                                               (+ POSITION_FLAG_PATH POSITION_FLAG_SOLUTION_PATH))
(const         PERIMETER_SEARCH_BACKWARD_SEARCH_TIME_PCT 30) ; perimeter search: spend this percentage of the time on the backward search
(const         SEARCH_STRATEGIES               '(backward forward perimeter))
(const         DEFAULT_TRANSPOSITION_TABLE_SIZE_LIMIT
                                               10000000)
(const         DEFAULT_TIME_LIMIT_SECONDS      60)
(const         DEFAULT_SEARCH_STRATEGY         'perimeter) ; backward, forward, or perimeter
(const         MOVES_LINE_LENGTH               70)

(const         TEXT_BACKWARD_SEARCH            "Backward search")
(const         TEXT_FORMAT_DISPLAY_STATUS      "~A: Time, seconds: ~D Positions: ~D Open: ~D Pushes: ~D")
(const         TEXT_FORMAT_OPTIMALITY          "Optimality: ~A")
(const         TEXT_FORMAT_SOLUTION_MOVES_PUSHES
                                               "Solution ~D/~D")
(const         TEXT_FORMAT_SOLVER              "Solver: ~A")
(const         TEXT_FORMAT_SOLVER_STATISTICS   "Solved levels: ~D of ~D   Moves: ~D   Pushes: ~D   Time, seconds: ~D")
(const         TEXT_FORMAT_SOLVER_TIME_SECONDS "Solver time, seconds:  ~D")
(const         TEXT_FORWARD_SEARCH             "Forward search")
(const         TEXT_INTERNAL_ERROR             "Internal error")
(const         TEXT_NO                         "no") 
(const         TEXT_OUT_OF_MEMORY              "Out of memory")
(const         TEXT_SOLUTION_PATH_CONSTRUCTION_FAILED
                                                "Constructing the solution path failed")
(const         TEXT_PROGRAM_NAME_AND_VERSION   "YAMSS 0.1")
(const         TEXT_PUSH_OPTIMAL               "Push optimal")
(const         TEXT_RECYCLED_POSITIONS         "Recycled positions:")
(const         TEXT_REPAIR_BOX_AND_DIRECTION_FAILED
                                                "Repairing box and direction for a position failed")
(const         TEXT_TIME                       "Time")
(const         TEXT_SEARCH_STATES 
               (list "Searching" "Search terminated" "Path inconsistency" "Pushes limit exceeded" 
                     "Time limit exceeded" "Transposition table full" "Not solved" "Solved"))
(const         TEXT_TRIVIAL_SOLUTION_LINE_1    "The puzzle has a trivial zero-pushes solution.")
(const         TEXT_TRIVIAL_SOLUTION_LINE_1_AFTER_PREPROCESSING
                                               "The puzzle has a trivial zero-pushes solution after preprocessing.")
(const         TEXT_TRIVIAL_SOLUTION_LINE_2    "The search for a non-trivial solution continues.")
(const         TEXT_UNSOLVABLE_LEVEL           "The puzzle is unsolvable.")
(const         TEXT_YES                        "yes")
)              ; end of (eval-when (:execute :load-toplevel :compile-toplevel compile load eval)

(var           a-box-blocked-on-a-non-goal-square? nil) ; a "freezing-move?" variable. global to ensure efficiency.
(var           A*-search?                      t) ; if false, then perform a greedy search instead of an A* search
(var           board                           nil)
(var           BOARD_CHAR_VALUE_MAP            nil)
(var           BOARD_VALUE_CHAR_MAP            nil)
(var           board-hash-value                0)
(var           board-height                    0)
(var           board-size                      0)
(var           board-width                     0)
(var           box-count                       0)
(var           box-squares                     nil)
(var           current-position                nil)
(var           DIRECTION_TO_TUNNEL_FLAG_MAP    nil)
(var           DIRECTION_TO_UPPERCASE_CHAR     nil)
(var           distance-queue-item-pool        nil)
(var           display-status-next-time-tick-count 0)
(var           display-status-time-interval-tick-count 0)
(var           forward-mode-start-position     nil)
(var           freezing-move-timestamp-boards-vector nil) ; one timestamp board for each axis    
(var           goal-count                      0)
(var           goal-squares                    nil)
(var           input-file-levels               nil) ; levels loaded from the input file. file name in "input-file-name".
(var           input-file-name                 nil) ; file with levels in "SOK" file format
(var           level-end-index                 nil) ; last  level to solve in input file. 1-based, inclusive.
(var           level-start-index               1)   ; first level to solve in input file. 1-based, inclusive.
(var           level-start-pusher-square       0)
(var           lookup-key                      nil)
(var           OPPOSITE_DIRECTION_MAP          nil)
(var           neighbor-square-offsets         nil)
(var           open-buckets-vector             nil)
(var           open-count                      0)
(var           open-max-value                  0) ; "open-max-value" is also the solver status
(var           open-min-value                  0)
(var           output-file-name                nil)
(var           pusher-reachable-squares-board-vector nil) ; a board-sized square stack or queue, not really a board
(var           pusher-reachable-squares-for-search-depth-vector nil) 
(var           pusher-square                   0)
(var           reverse-mode?                   nil)
(var           search-best-pruned-score        0)
(var           search-count-dequeued-open-positions 0)
(var           search-count-duplicate-positions 0)
(var           search-count-new-best-paths     0)
(var           search-count-positions          0)   
(var           search-count-pushes             0)
(var           search-count-path-repairs       0)    
(var           search-count-recycled-positions 0)
(var           search-debug-path               nil)
(var           search-level                    0) ; currently processed level
(var           search-limit-depth              0) ; maximum number of pushes for a solution
(var           search-limit-pushes             0) ; maximum number of generated box moves
(var           search-limit-score              0)
(var           search-limit-stop-when-solved?  t)
(var           search-limit-time-seconds       DEFAULT_TIME_LIMIT_SECONDS)
(var           search-limit-time-ticks         0)
(var           search-no-progress-rover        0) 
(var           search-node-recycling-enabled?  nil)
(var           search-perimeter-list           nil)
(var           search-start-time-tick-count    0)
(var           search-strategy                 DEFAULT_SEARCH_STRATEGY)
(var           search-time-milliseconds        0)
(var           search-solution-moves           nil)  
(var           search-solution-position        nil)
(var           simple-lower-bound              0)
(var           square-nearest-goal-distances   nil)
(var           square-nearest-start-distances  nil)
(var           timestamp-board-pool            nil)
(var           transposition-table             nil)
(var           transposition-table-add-result-type nil) ; "transposition-table-add" secondary return value
(var           transposition-table-item-count  0)
(var           transposition-table-items       nil) ; all transposition table items, both free and in use
(var           transposition-table-item-pool   nil) ; free items
(var           transposition-table-lookup-first-item-with-hash-value nil) ; "transposition-table-lookup" secondary return value
(var           transposition-table-size-limit  DEFAULT_TRANSPOSITION_TABLE_SIZE_LIMIT)
(var           tube-filling-moves              nil)
(var           zobrist-bit-strings             nil)
(var           zobrist-bit-strings-random-seed 0) ; local random function seed, for reproducible results

(struct move
  (direction 0 :type fixnum)
  (box-index)) ; box-index is nil for non-pushing moves

(mc .height             (board)        `(aref ,board                 ,BOARD_INDEX_HEIGHT   ))
(mc .board-pusher-square ; a bit of a misnomer. the game pusher square is in "pusher-square", 
                         ; not in the board vector. the pusher square in board vectors is 
                         ; typically used for storing a normalized pusher square value.
                        (board)        `(aref ,board                 ,BOARD_INDEX_PUSHER   ))
(mc .timestamp          (board)        `(aref ,board                 ,BOARD_INDEX_TIMESTAMP))
(mc .width              (board)        `(aref ,board                 ,BOARD_INDEX_WIDTH    ))

(mc squares             (index)        `(aref board                  ,index))
(mc boxes               (index)        `(aref box-squares            ,index))
(mc goals               (index)        `(aref goal-squares           ,index))
(mc opposite-direction  (direction)    `(aref OPPOSITE_DIRECTION_MAP ,direction))
(mc direction-to-axis   (direction)    `(bitand ,direction           AXIS_MASK))
(mc direction-to-tunnel-flag-value
                        (direction)    `(bit-shift-arithmetic 1      ,direction))
(mc pusher-reachable-squares-for-search-depth             
                        (depth)        `(aref pusher-reachable-squares-for-search-depth-vector
                                              ,depth))
(mc object-bits         (value)        `(bitand BOARD_OBJECT_BITS    ,value))
(mc box-value?          (value)        `(bit? BOX_BIT                ,value))
(mc box-start-square-value?
                        (value)        `(bit? BOX_START_SQUARE_BIT   ,value))      
(mc empty-value?        (value)        `(zero? (object-bits          ,value)))
(mc empty-floor-value?  (value)        `(equal? FLOOR_VALUE 
                                                (object-bits         ,value)))
(mc floor-value?        (value)        `(bit? FLOOR_BIT              ,value))
(mc goal-value?         (value)        `(bit? GOAL_BIT               ,value))
(mc pusher-value?       (value)        `(bit? PUSHER_BIT             ,value))
(mc wall-value?         (value)        `(bit? WALL_BIT               ,value))
(mc gate-value?         (value)        `(bit? GATE_BIT               ,value))
(mc visited-value?      (value)        `(bit? VISITED_BIT            ,value))
(mc tunnel-square-value?
                        (value direction)
                                       `(bit? ,direction             ,value))  
(mc box-on-goal-value?  (value)        `(and (box-value?             ,value)
                                             (goal-value?            ,value)))

(mc box-square?         (square)       `(box-value?         (squares ,square)))
(mc box-on-goal-square? (square)       `(box-on-goal-value? (squares ,square)))
(mc box-start-square?   (square)       `(box-start-square-value?
                                                            (squares ,square)))
(mc empty-square?       (square)       `(empty-value?       (squares ,square)))
(mc empty-floor-square? (square)       `(empty-floor-value? (squares ,square)))
(mc floor-square?       (square)       `(floor-value?       (squares ,square)))
(mc goal-square?        (square)       `(goal-value?        (squares ,square)))
(mc pusher-square?      (square)       `(pusher-value?      (squares ,square)))
(mc wall-square?        (square)       `(wall-value?        (squares ,square)))
(mc gate-square?        (square)       `(gate-value?        (squares ,square)))
(mc visited-square?     (square)       `(visited-value?     (squares ,square)))

(mc tunnel-square?      (square direction)
  `(tunnel-square-value? (squares ,square) ,direction))  

(mc legal-box-square?   (square)       
  `(equal? FLOOR_VALUE
           (bitand (squares ,square)
                   WALL+ILLEGAL_BOX_SQUARE+UNREACHABLE_BOX_SQUARE+FLOOR_VALUE)))

(mc legal-box-square-with-no-box? (square)       
  `(equal? FLOOR_VALUE
           (bitand (squares ,square)
                   WALL+BOX+ILLEGAL_BOX_SQUARE+UNREACHABLE_BOX_SQUARE+FLOOR_VALUE)))

;(mc box-start-square?   (square)      
;  `(zero? (aref square-nearest-start-distances ,square))) 

(mc set-square-bits!    (square bit-value)
  `(set! (squares ,square) (bitor (squares ,square) ,bit-value)))

(mc clear-square-bits!  (square bit-value)
  `(dec! (squares ,square) (bitand (squares ,square) ,bit-value)))

(mc neighbor-square     (square direction)
  `(+ ,square (aref neighbor-square-offsets ,direction)))

(mc neighbor-square-value (square direction)
  `(squares (neighbor-square ,square ,direction)))    

(mc normalized-pusher-square (timestamp-board)
  `(aref ,timestamp-board BOARD_INDEX_PUSHER))

(mc reachable-floor? (square timestamp-board)
  `(equal? (.timestamp ,timestamp-board)
           (aref ,timestamp-board ,square)))

(mc reachable-box? (square timestamp-board)
  `(equal? (1+ (.timestamp ,timestamp-board))
           (aref ,timestamp-board ,square)))

(mc distance-to-nearest-target (square)
  `(if reverse-mode?
       (aref square-nearest-start-distances ,square)
       (aref square-nearest-goal-distances  ,square)))

(mc move-pusher (to-square)
  `(do.. (dec! (squares pusher-square) PUSHER)
         (inc! (squares ,to-square   ) PUSHER)
         (set! pusher-square           ,to-square)))

(fn move-box (box-index to-square)
  (do.. (inc! simple-lower-bound 
              (- (distance-to-nearest-target to-square)
                 (distance-to-nearest-target (boxes box-index))))
        (set! board-hash-value
              (bitxor (bitxor board-hash-value 
                              (aref zobrist-bit-strings (boxes box-index)))
                      (aref         zobrist-bit-strings to-square)))
        (dec! (squares (boxes box-index)) BOX)
        (inc! (squares to-square        ) BOX)    
        (set! (boxes   box-index)         to-square)))

(mc open-buckets        (index)        `(aref open-buckets-vector ,index))
(mc open-bucket-empty?  (index)        `(not (open-buckets ,index)))

(mc position-free? (position)      
  `(bit? POSITION_FLAG_FREE_BIT (.flags ,position)))

(mc position-on-path? (position)      
  `(bit? POSITION_FLAG_PATH_BIT (.flags ,position)))

(mc position-on-path-or-solution-path?   (position)      
  `(not (zero? (bitand POSITION_FLAGS_PATH+SOLUTION_PATH (.flags ,position)))))

(mc position-on-perimeter?  (position)      
  `(bit? POSITION_FLAG_PERIMETER_BIT (.flags ,position)))

(mc position-open?      (position)      
  `(bit? POSITION_FLAG_OPEN_BIT (.flags ,position)))

(fn col-row-to-square (col row board-width)
  (+ (1+ col) (* (+ board-width 2) (+ row 2))))

(fn square-to-col-row (square board-width)
  (let* ((row (truncate (/ square  (+ board-width 2))))
         (col (- square (* row     (+ board-width 2)))))
    (values (1- col) (- row 2))))

(fn board-char-to-square-value (char)
  (aref BOARD_CHAR_VALUE_MAP (min MAX_ASCII (char-int char))))

(fn square-value-to-board-char (value)
  (aref BOARD_VALUE_CHAR_MAP (bit-shift-arithmetic (object-bits value) (- DIRECTION_COUNT))))

(fn square-index-to-text (square)
  (multiple-value-bind (col row) (square-to-col-row square board-width)
    (if (<= board-width 26)
        (format nil "~36R~10,v,'0R" (+ 10 col) (ceiling (log (1+ board-height) 10)) (1+ row))
        (format nil "~10,v,'0R:~10,v,'0R"      (ceiling (log (1+ board-width ) 10)) (1+ col) (ceiling (log (1+ board-height) 10)) (1+ row)))))

(mc for-each-square (square &rest body)
  (let ((col (new-symbol)) 
        (row (new-symbol)))
    `(let (,square)
       (dotimes (,row board-height)
         (dotimes (,col board-width)
           (set! ,square (col-row-to-square ,col ,row board-width))
           ,@body)))))

(mc for-each-floor-square (square &rest body)
  `(dotimes (,square  (length board))
     (if (and (> ,square BOARD_INDEX_TIMESTAMP) ; skip header fields, width, heigth, etc.
              (floor-square? ,square))
         (do.. ,@body))))  

(mc for-each-neighbor-square (square neighbor &rest body)
  (let ((direction (new-symbol)))
    `(let (,neighbor)
       (for-each-direction ,direction
         (set! ,neighbor (neighbor-square ,square ,direction))
         ,@body))))

(mc for-each-integer-in-series (index count &rest body)
  `(dotimes (,index ,count) ,@body))
  
(mc for-each-direction (direction  &rest body)  `(for-each-integer-in-series ,direction  DIRECTION_COUNT ,@body))

(mc for-each-axis      (axis       &rest body)  `(for-each-integer-in-series ,axis       AXIS_COUNT      ,@body))
  
(mc for-each-box       (box-index  &rest body)  `(for-each-integer-in-series ,box-index  box-count       ,@body))

(mc for-each-goal      (goal-index &rest body)  `(for-each-integer-in-series ,goal-index goal-count      ,@body))

(mc with-timestamp-board (variable-name-and-initialization-form &rest body) 
  ; for simplicity and efficiency, the return value is undefined. 
  ; it's not the value returned by the forms in "body".
  `(let ((,(first variable-name-and-initialization-form) 
          ,(or (second variable-name-and-initialization-form) 
               '(get-timestamp-board-from-pool)))) 
        ,@body
        (release-timestamp-board-to-pool ,(first variable-name-and-initialization-form))))

(mc freezing-move-timestamps (axis)
  `(aref freezing-move-timestamp-boards-vector ,axis))

(fn wall-neighbors (square)
  (let ((result 0))
    (for-each-neighbor-square square neighbor
      (if (wall-square? neighbor)
          (inc! result)))
    result))

(fn dead-end-square? (square)
  (>= (wall-neighbors square) (1- DIRECTION_COUNT)))

(fn legal-box-square-neighbors (square)
  (let ((result 0))
    (for-each-neighbor-square square neighbor
      (if (legal-box-square? neighbor)
          (inc! result)))
    result))

(fn search-state-to-text (search-state)
  ; precondition: the search state is either a non-negative integer, indicating that
  ; the search is in progress, or it's one of the special "..._SCORE" constants, 
  ; e.g., "TIME_LIMIT_EXCEEDED_SCORE", which tells why the search has terminated.
  ; the global value "open-max-value" contains the current search state for the solver.
  (nth (abs (min 0 search-state)) TEXT_SEARCH_STATES))

(mc direction-to-char (direction)
  `(aref DIRECTION_TO_UPPERCASE_CHAR ,direction))

(fn move-to-char (move)
  (if (move-box-index move)
      (aref DIRECTION_TO_UPPERCASE_CHAR (move-direction move))
      (aref DIRECTION_TO_LOWERCASE_CHAR (move-direction move))))

(fn moves-to-text (moves)
  (let ((result (make-string (length moves) :initial-element #\space))
        (index  0))
    (dolist (move moves)
      (set! (aref result index) (move-to-char move))
      (inc! index))
    result ))

(fn moves-to-text-lines (moves &optional (line-length MOVES_LINE_LENGTH))
  (if (list? moves) ; otherwise, moves is assumed to be a string 
      (set! moves (moves-to-text moves)))
  (let ((result      nil)
        (text-length (length moves))
        (index       0))
    (while (< index text-length)
      (push (subseq moves index (min (+ index line-length) text-length)) result)
      (inc! index line-length))
    (reverse! result)))

(fn move-list-move-count (moves) ; returns the number of moves on the list of moves
  (if (list? moves) ; true: not a zero-pushes trivial solution (in which case "moves" is "t")
      (length moves)
      0))

(fn move-list-push-count (moves) ; returns the number of pushes on the list of moves
  (if (list? moves) ; true: not a zero-pushes trivial solution (in which case "moves" is "t")
      (let ((result 0))
        (dolist (move moves)
          (if (move-box-index move)
              (inc! result)))
        result)
      0)) 

(fn box-at-square (square) ; returns the index of the box at the given square
  (let ((result nil))
    (for-each-box index
      (if (equal? (boxes index) square)
          (set! result   index)))
    result))

(fn goal-at-square (square) ; returns the index of the goal at the given square
  (let ((result nil))
    (for-each-goal index
      (if (equal? (goals index) square)
          (set! result   index)))
    result))

(fn delete-box (box-index)
  (clear-bit! BOX_BIT (squares (boxes box-index)))
  (for-each-box index
    (if (> index box-index)
        (set! (boxes (1- index)) (boxes index))))
  (dec! box-count))

(fn delete-goal (goal-index)
  (clear-bit! GOAL_BIT (squares (goals goal-index)))
  (for-each-goal index
    (if (> index goal-index)
        (set! (goals (1- index)) (goals index))))
  (dec! goal-count))

(fn backward-search-box-blocked-on-square? (square timestamp) ; local function for "freezing-move?". global to ensure efficiency.
  (let ((square-timestamp-value (aref (freezing-move-timestamps 0) square)))
    (if (>= square-timestamp-value timestamp) ; true: the result has already been calculated for this square, or it's a wall
        (>  square-timestamp-value timestamp) ; >   : a box is blocked at this square  
        ; the result for this square has not been calculated yet. do it now.
        (let ((result       t) ; t: the box is considered blocked until it has been proved that it isn't
              (box-on-non-target-square? nil)
              (queue        pusher-reachable-squares-board-vector) ; a board-sized square stack or queue, not really a board
              (queue-bottom square) ; first square on the queue
              (queue-top    square) ; last  square on the queue
              neighbor)
          
          ; the visited squares are not enqueued by storing them sequentially in vector elements 0, 1, 2, etc. instead,
          ; visited squares are linked together using the vector elements given by the square numbers. each square is visited 
          ; only once, no matter at which recursion level it's visited, so implementing the queue this way makes it possible 
          ; to get by with just one single board-sized vector, even though each recursive call to the function creates its 
          ; own queue.
          
          (inc! (squares square)       WALL) ; turn the square into a wall, so it isn't visited more than once            
          (set! (aref queue queue-top) 0)    ; 0: end of queue. so far, only the specified square has been put on the queue.
          
          (while (not (zero? square)) ; for each square on the queue
            
            (if (not (box-start-square? square))
                ; if boxes turn out to be blocked, then it's necessary also to know
                ; whether a box is blocked on a non-target square
                (set! box-on-non-target-square? t))
            
            (for-each-direction direction
              (if result ; true: not proved that a box on the square can move
                  (do.. (set! neighbor (neighbor-square square direction))
                        (if (and (not (wall-square?     neighbor))
                                 (or  (not (box-square? neighbor)) 
                                      (not (backward-search-box-blocked-on-square? neighbor timestamp)))) 
                            (do.. ; the neighbor square is neither a wall not a blocked box. 
                                  ; advance to the next square in this direction.
                                  (set! neighbor (neighbor-square neighbor direction))
                                  (if (not (wall-square? neighbor))    
                                      ; the square is neither a wall nor a visited box square with a temporary wall
                                      (if (box-square? neighbor)
                                          (do.. ; the square is an unvisited box square. put in on the queue.
                                                (set! (aref queue queue-top) neighbor)
                                                (set! queue-top              neighbor)
                                                (set! (aref queue queue-top) 0) ; 0: end of queue
                                                ; turn the square into a wall, so it isn't visited more than once
                                                (inc! (squares neighbor)     WALL))
                                          ; the square is an empty floor, hence, the boxes can move
                                          (set! result nil)))))))) ; nil: exit direction loop and return the value                                 

            (if result ; true: not proved that a box can move  
                (set! square (aref queue square)) ; advance to next item on the queue
                (set! square 0))) ; exit while loop             
          
          (set! result (and result box-on-non-target-square?))
          (set! a-box-blocked-on-a-non-goal-square? result) ; update and return value to "freezing-move?"
          
          ; remove temporary walls from all the visited squares, and dampen exponential growth by 
          ; saving the result for all the visited squares
          (set! square queue-bottom)
          (while (not (zero? square))
            (dec! (squares square) WALL)            
            (set! (aref (freezing-move-timestamps 0) square) (+ timestamp (boolean-to-integer result))) 
            (set! square (aref queue square))) ; advance to next item on the queue
          
          result)))) 

(fn forward-search-box-blocked-along-axis? (axis square timestamp) ; local function for "freezing-move?". global to ensure efficiency.            
                  (let ((square-timestamp-value (aref (freezing-move-timestamps axis) square)))
                    (if (>= square-timestamp-value timestamp) ; true: the result has already been calculated for this [square x axis], or it's a wall
                        (>  square-timestamp-value timestamp) ; >   : a box is blocked at this square along this axis
                        ; the result for this [square x axis] has not been calculated yet. do it now.
                        (let* ((neighbor1 (neighbor-square square axis))
                               (neighbor2 (neighbor-square square (+ axis AXIS_COUNT))) ; opposite direction
                               (result    (or (wall-square? neighbor1)
                                              (wall-square? neighbor2)
                                              (and (not (legal-box-square? neighbor1))
                                                   (not (legal-box-square? neighbor2))))))
                          (inc! (squares square) WALL)
                          
                          ; it's not enough to know whether a box is blocked.
                          ; it's also necessary to know whether a box is blocked on a non-goal square.
                          (if (and (not (and result             a-box-blocked-on-a-non-goal-square?))
                                   (not (wall-square?           neighbor1)) 
                                   (box-square?                 neighbor1)
                                   (forward-search-box-blocked-along-all-axis? 0 neighbor1 timestamp))
                                   (set! result                 t))  
                          (if (and (not (and result             a-box-blocked-on-a-non-goal-square?))
                                   (not (wall-square?           neighbor2)) 
                                   (box-square?                 neighbor2)
                                   (forward-search-box-blocked-along-all-axis? 0 neighbor2 timestamp))
                                   (set! result                 t))            
          
                          (if (and result (not (goal-square?    square)))
                              (set! a-box-blocked-on-a-non-goal-square? t))
                          
                          ; store the result for this [square x axis] in the timestamp vector. this is necessary to 
                          ; reduce the exponential growth which otherwise could overwhelm the recursive calls.                         
                          (set! (aref (freezing-move-timestamps axis) square) (+ timestamp (boolean-to-integer result))) 
                          
                          (dec! (squares square) WALL)
                          result))))
                
(fn forward-search-box-blocked-along-all-axis? (axis square timestamp) ; local function for "freezing-move?". global to ensure efficiency.
                  (or (equal? axis AXIS_COUNT) ; true: all axis have been tested positive        
                      (and    (forward-search-box-blocked-along-axis?         axis  square timestamp)
                              (forward-search-box-blocked-along-all-axis? (1+ axis) square timestamp))))

(fn freezing-move? (from-square to-square)
  ; returns "true" if a box positioned at "to-square" makes one or more boxes freeze
  ; on a non-goal square, i.e., causing a deadlock. if "from-square" if non-nil, 
  ; then the box is removed from "from-square" before the calculation.
  ; precondition: "to-square" is a legal box square with no box.
  (let* (result
         timestamp
         (original-from-square-value          (if from-square (squares from-square)))) 
    (if (>= (.timestamp (freezing-move-timestamps 0)) MAX_TIMESTAMP)
        (for-each-axis axis 
          (initialize-timestamp-board (freezing-move-timestamps axis))))    
    (set! timestamp                           (inc! (.timestamp (freezing-move-timestamps 0)) 2))
    (set! a-box-blocked-on-a-non-goal-square? nil) ; updated by "forward-search-box-blocked-along-axis?" and "backward-search-box-blocked-on-square?"
    (if   from-square
          (clear-bit! BOX_BIT (squares from-square)))
    (if   reverse-mode?
          (set! result (or ;(not (legal-box-square? to-square))     
                           (and (backward-search-box-blocked-on-square? to-square timestamp)
                                a-box-blocked-on-a-non-goal-square?)))        
          (set! result (or ;(not (legal-box-square? to-square))     
                           (and (forward-search-box-blocked-along-all-axis? 0 to-square timestamp)
                                a-box-blocked-on-a-non-goal-square?))))
    ;(if   result
    ;      (do.. (display "FROZEN" (square-index-to-text to-square)) 
    ;            (set-bit! BOX_BIT (squares to-square)) (display-board) (clear-bit! BOX_BIT (squares to-square)) 
    ;            (read-line)))
    (if   from-square
          (set! (squares from-square) original-from-square-value))
    result))

(fn remove-boxes-from-board () 
  ; removes boxes from the board without updating other information, such as the simple lower bound
  (for-each-box index (dec! (squares (boxes index)) BOX)))
 
(fn put-boxes-on-board ()
  ; puts boxes on the board without updating other information, such as the simple lower bound  
  (for-each-box index (inc! (squares (boxes index)) BOX)))

(fn mark-gate-squares-on-board ()
  (let ((original-pusher-square pusher-square)
        neighbor
        opposite-neighbor) 
    (with-timestamp-board (reachable)    
      (remove-boxes-from-board)
      (for-each-square square
        (if (legal-box-square? square)
            (do.. (set! pusher-square 0)
                  (for-each-axis axis
                    (set! neighbor          (neighbor-square square                     axis))
                    (set! opposite-neighbor (neighbor-square square (opposite-direction axis)))
                    ; only simple gate squares are recognized, where a box at the gate square
                    ; only can move along one axis. the tunnel square calculation may depend on
                    ; this restriction.
                    (if (and  (floor-square? neighbor) 
                              (floor-square? opposite-neighbor)
                              (not (and (bit? ILLEGAL_BOX_SQUARE_BIT (squares neighbor))
                                        (bit? ILLEGAL_BOX_SQUARE_BIT (squares opposite-neighbor)))))
                        (if (zero? pusher-square)
                            (set!  pusher-square neighbor)
                            (set!  pusher-square -1))))
                   (if (> pusher-square 0) ; true: a box at the square can only move along one axis
                       (do.. (inc! (squares square) BOX)
                             (calculate-pusher-reachable-squares reachable)
                             (dec! (squares square) BOX)  
                             (for-each-neighbor-square square  neighbor
                               (if (and (floor-square?         neighbor)                    
                                        (not (reachable-floor? neighbor reachable))
                                        (not (gate-square?     square)))
                                   (inc! (squares square) GATE_SQUARE))))))))
      (put-boxes-on-board)
      (set! pusher-square original-pusher-square))))  

(fn mark-tunnel-squares-on-board (reverse-mode?)
  (local-fns (
    (box-can-move-in-this-direction-on-an-empty-board? (square direction push?)
      (let* ((box-to-square  (neighbor-square square direction))
             (pusher-square  (if push?
                                 (neighbor-square square (opposite-direction direction))
                                 (neighbor-square box-to-square direction))))
        (and (floor-square? pusher-square)
             (floor-square? square)
             (floor-square? box-to-square)
             (not (bit? ILLEGAL_BOX_SQUARE (squares box-to-square))))))
             
    (box-can-move-along-this-axis-on-an-empty-board?        (square                     direction  push?)
      (or (box-can-move-in-this-direction-on-an-empty-board? square                     direction  push?)  
          (box-can-move-in-this-direction-on-an-empty-board? square (opposite-direction direction) push?)))
             
    (box-can-move-along-another-axis-on-an-empty-board? (square direction push?)
      (let ((result nil)
            (axis (direction-to-axis direction)))
        (for-each-axis another-axis
          (if (and (not-equal? another-axis axis)
                   (box-can-move-along-this-axis-on-an-empty-board? square another-axis push?))
              (set! result t)))
        result)))
    
    (let (neighbor
          this-square-blocked-along-all-other-axis-by-walls?) 
      (for-each-square square 
        (clear-square-bits! square TUNNEL_FLAG_BITS))
      (for-each-square square
        (if (legal-box-square? square)
            (for-each-direction direction
              (set! neighbor (neighbor-square square direction))
              (if (legal-box-square? neighbor)
                  (do.. (set! this-square-blocked-along-all-other-axis-by-walls?
                              (and (floor-square? (neighbor-square square (opposite-direction direction)))
                                   (equal? (wall-neighbors square) (- DIRECTION_COUNT 2))))
                        (if reverse-mode?
                            (if (and (not (box-start-square? neighbor))
                                     (floor-square? (neighbor-square neighbor direction))                                 
                                     (or  this-square-blocked-along-all-other-axis-by-walls?
                                          (gate-square? neighbor))
                                     (not (box-can-move-along-another-axis-on-an-empty-board? neighbor direction nil)))                         
                                (set-square-bits! neighbor (direction-to-tunnel-flag-value direction)))
                            (if (and (not (goal-square? neighbor))
                                     (floor-square? (neighbor-square square (opposite-direction direction))))                                 
                                (if  (or (and (or this-square-blocked-along-all-other-axis-by-walls?
                                                  (gate-square? neighbor))
                                              (not (box-can-move-along-another-axis-on-an-empty-board? neighbor direction t)))
                                         (and this-square-blocked-along-all-other-axis-by-walls?
                                              (gate-square? square)))
                                     (set-square-bits! neighbor (direction-to-tunnel-flag-value direction))))))))))
      ;(if reverse-mode? (display "reverse mode tunnels") (display "forward mode tunnels"))
      ;(display-board board 0 TUNNEL_FLAG_BITS) (nl)
    )))

(fn fill-tubes-on-board ()
  (let ((tube-filling-moves nil)
        (box-on-goal-count 0)
        (more? t)
        dead-end? 
        move-direction
        new-pusher-square
        new-box-square
        neighbor
        index)
    (for-each-box index ; calculate the number of boxes at goal squares
      (if (goal-square? (boxes index))
          (inc! box-on-goal-count)))          
    (while more?
      (set! more? nil)
      
      ; delete immovable boxes at goal squares, if any
      (set! index box-count)
      (while (> index 0)
        (dec! index)
        (if (and  (goal-square?           (boxes index))
                  (or (dead-end-square?   (boxes index))
                      (not (floor-square? (boxes index))))) ; not floor square: not reachable for the pusher
            (do.. (delete-goal            (goal-at-square (boxes index)))  
                  (delete-box             index)
                  (dec!                   box-on-goal-count)
                  (set! more?             t))))
      
      ; fill tubes, if any
      (for-each-square square
        (if (and (equal? FLOOR_VALUE (bitand (squares square) BOX+GOAL+FLOOR_VALUE))            
                 (dead-end-square? square))
            (do.. (set! dead-end?         (not-equal? square pusher-square)) 
                  (set! new-pusher-square nil)
                  (set! new-box-square    nil)
                  (for-each-direction direction
                    (set! neighbor (neighbor-square square direction))
                    (if (floor-square? neighbor)
                        (do.. (set! move-direction direction)
                              (if (equal? square pusher-square) ; true: the pusher is at this square, which only has one floor square neighbor
                                  (do.. (set! new-pusher-square neighbor)
                                        (if (box-square? neighbor) ; true: there is a box at this square. check if it can be pushed.
                                            (do.. (set! new-box-square (neighbor-square neighbor direction))
                                                  (set! dead-end? ; true: the box can be pushed, and it's not a solution position
                                                        (and (zero? (bitand (squares new-box-square) 
                                                                            WALL+BOX+ILLEGAL_BOX_SQUARE))
                                                             (not (equal? box-on-goal-count box-count)))))
                                            (set! dead-end? (not-equal? box-on-goal-count box-count))))))))
                  (if dead-end?
                      (do.. (if new-pusher-square ; true: the pusher is at this dead end square. move the pusher before putting a wall at the square.
                                (do.. (move-pusher new-pusher-square)
                                      (set! level-start-pusher-square pusher-square) 
                                      (push (make-move :direction move-direction :box-index nil) tube-filling-moves)
                                      (if new-box-square ; true: a box is pushed when the pusher moves in the only open direction
                                          (let ((index            (box-at-square new-pusher-square)))
                                            (dec! (squares        new-pusher-square) BOX)  
                                            (inc! (squares        new-box-square   ) BOX)    
                                            (set! (boxes index)   new-box-square)
                                            (if (goal-square?     new-pusher-square)
                                                (dec!             box-on-goal-count))
                                            (if (goal-square?     new-box-square)
                                                (inc!             box-on-goal-count))                                                 
                                            (set! (move-box-index (first tube-filling-moves)) index)))))
                            (set! (squares square) WALL)
                            (set! more? t)))))))
    
    ; after tube-filling, the box starting positions can be marked directly on the board
    (for-each-box index
      (set-bit! BOX_START_SQUARE_BIT (squares (boxes index))))        
    
    ;(display "tube-filling moves " (reverse tube-filling-moves)) (nl)
    (reverse! tube-filling-moves)))

(fn board-to-text-lines (board &optional right-shift bit-mask)
  (let ((lines nil)
        (width       (.width  board))
        (height      (.height board))
        (bit-mask    (if bit-mask bit-mask MOST-POSITIVE-FIXNUM))
        line
        square
        value)
    (dotimes (row height)
      (if right-shift
          (do.. (set! line "")
                (dotimes (col width)
                  (set! square (col-row-to-square col row width))
                  (set! value  (bitand bit-mask (bit-shift-arithmetic (aref board square) (- right-shift))))
;                 (if (<= 0 value 999999999)
;                     (set! line (concatenate 'string line (format nil " ~9d" value)))                  
                  (if (<= 0 value 9999)
                      (set! line (concatenate 'string line (format nil " ~4d" value)))
                      (set! line (concatenate 'string line "    *")))))
          (do.. (set! line (make-string width :initial-element BOARD_CHAR_FLOOR1))
                (dotimes (col width)
                  (set! (aref line col) 
                        (square-value-to-board-char (aref board (col-row-to-square col row width)))))))
      (push line lines))
    (reverse! lines)))

(fn display-board (&optional (board board) right-shift bit-mask)
  (dolist (line (board-to-text-lines board right-shift bit-mask))
    (displayln line))
  (nl))

(fn fill-board (board square-value)
  (let ((width (.width board)))
    (dotimes (col width)
      (dotimes (row (.height board))
        (set! (aref board (col-row-to-square col row width)) square-value))))
  board)

(fn make-board (width height)
  (let ((board (make-array (round-up-to-even (* (+ 2 width) (+ 3 height))) :element-type 'fixnum :initial-element WALL)))
    (set! (.width  board)              width)
    (set! (.height board)              height)
    (set! (.board-pusher-square board) 0)
    (set! (.timestamp board)           MAX_TIMESTAMP)    
    (fill-board board                  FLOOR_VALUE)))

(fn load-board (new-board)
  (let ((box-list    nil)
        (goal-list   nil)
        (pusher-list nil))
    (set! search-start-time-tick-count (get-tick-count)) ; solver time statistics must include level preprocessing
    (set! board                        new-board)
    (set! board-width                  (.width  board))
    (set! board-height                 (.height board))
    (set! board-size                   (* board-width board-height))
    
    (for-each-square square
      (if (pusher-square? square)
          (push square pusher-list))
      (if (box-square? square)
          (push square box-list)) 
      (if (goal-square? square)
          (push square goal-list)))
    (set! pusher-list (reverse! pusher-list))
    (set! box-list    (reverse! box-list))
    (set! goal-list   (reverse! goal-list))

    (if pusher-list
        (set! pusher-square (pop pusher-list))
        (set! pusher-square 0))
    (set! level-start-pusher-square pusher-square)
    (set! box-count      (min (length box-list) (length goal-list)))
    (set! goal-count     box-count)
    (set! box-squares    (make-array box-count  :element-type 'fixnum))
    (set! goal-squares   (make-array goal-count :element-type 'fixnum))
    (for-each-box index
      (set! (boxes index) (pop box-list)))
    (for-each-goal index
      (set! (goals index) (pop goal-list)))
    
    ; remove surplus objects
    (dolist (square pusher-list)
      (dec! (squares square) PUSHER))
    (dolist (square box-list)
      (dec! (squares square) BOX))
    (dolist (square goal-list)
      (dec! (squares square) GOAL))
    
    (set! (aref neighbor-square-offsets DIRECTION_UP   ) (- (+ board-width 2)))
    (set! (aref neighbor-square-offsets DIRECTION_LEFT ) -1)
    (set! (aref neighbor-square-offsets DIRECTION_DOWN )    (+ board-width 2))
    (set! (aref neighbor-square-offsets DIRECTION_RIGHT)  1)        
    
    (if (not (freezing-move-timestamps 0))
        (for-each-axis axis  
          (set! (freezing-move-timestamps axis) (initialize-timestamp-board nil))))
    (if (not  pusher-reachable-squares-board-vector)
        (set! pusher-reachable-squares-board-vector (get-timestamp-board-from-pool)))
    ; "pusher-reachable-squares-board-vector" is never recycled and cannot be recycled
    ; without a new initialization because it's used as a stack or a queue, not as a board.
    
    (if (not (zero? pusher-square))
        (do.. (remove-boxes-from-board)
              (with-timestamp-board (reachable (calculate-pusher-reachable-squares nil))
                (for-each-floor-square square
                  (if (not (reachable-floor? square reachable))
                      (clear-square-bits! square FLOOR_VALUE))))
              (put-boxes-on-board)
              (set! tube-filling-moves             (fill-tubes-on-board))
              (set! square-nearest-start-distances (calculate-nearest-start-or-goal-distances t   t nil)) ;pusher-square))
              (set! square-nearest-goal-distances  (calculate-nearest-start-or-goal-distances nil t nil))
              (set! simple-lower-bound             (calculate-simple-lower-bound))
              (set! board-hash-value               (calculate-board-hash-value board box-count box-squares))                              
              (mark-gate-squares-on-board)
              ;(mark-tunnel-squares-on-board t)
              ;(mark-tunnel-squares-on-board nil)
              board))))

(fn make-zobrist-bit-string (bit-size)
  (let ((result                 0)
        (low-limit              (bit-shift-arithmetic 1 (bit-shift-arithmetic bit-size -1)))
        (high-limit             (bit-shift-arithmetic 1 bit-size))
        (original-random32-seed random32-seed))
    ; uses a private random function and seed for reproducible results
    (set! random32-seed zobrist-bit-strings-random-seed)
    (while (< result low-limit)
           (let ((bit-shift 0 ))
             (while (< bit-shift bit-size)
                (set! result (mod (+ result 
                                     (bit-shift-arithmetic (random32) bit-shift)) 
                                  high-limit))
                (inc! bit-shift (if (zero? bit-shift) 8 16))))
           (dotimes (index      (fill-pointer zobrist-bit-strings)) ; ensure uniqueness
                   (if (equal?  result (aref  zobrist-bit-strings index))
                       (set!    result 0))))
    (set! zobrist-bit-strings-random-seed random32-seed)
    (set! random32-seed original-random32-seed)
    result)) 

(fn calculate-board-hash-value (board &optional box-count box-squares)
  (let ((width  (.width  board))
        (height (.height board))
        (result 0))
    (while (<= (fill-pointer zobrist-bit-strings) (length board))
      (vector-push-extend (make-zobrist-bit-string ZOBRIST_BIT_STRING_BIT_SIZE) zobrist-bit-strings))
    (if (and box-count box-squares)
        (dotimes (index box-count)
          (set! result (bitxor result (aref zobrist-bit-strings (aref box-squares index)))))
       (dotimes (row height)
         (dotimes (col width)
           (let ((square (col-row-to-square col row width)))
             (if (box-value? (aref board square))
                 (set! result (bitxor result (aref zobrist-bit-strings square))))))))
    result))

(fn calculate-simple-lower-bound ()
  (let ((result 0))
    (for-each-box index
      (let ((square (boxes index)))
        (if (not (zero? square)) ; true: box not temporarily removed from the board
            (inc! result (distance-to-nearest-target square)))))
    result))

(struct solution
  name          
  move-count    
  push-count    
  moves          ; list of moves
  notes          ; list of text strings
  time)          ; milliseconds

(struct level 
  index
  name
  board
  notes          ; list of text strings
  solution)

(fn load-levels-from-file (file-name)
  (local-fns ((board-line? (line)
               (let ((index (length line))   
                     (start nil)
                     (end nil)
                     value)
                 (while (> index 0)
                   (dec! index)
                   (if (not-equal? EMPTY_SQUARE (set! value (board-char-to-square-value (aref line index))))
                       (if (not (empty-floor-value? value))
                           (do.. (if (not end) 
                                     (set! end (1+ index)))
                                 (set! start index)))
                       (if (char< (aref line index) #\Space)
                           (set!  (aref line index) BOARD_CHAR_FLOOR1)                  
                           (set! index -1))))
                 (and (zero? index) 
                      start
                      (>= (- end start) MIN_BOARD_WIDTH)
                      (or (wall-value? (set!  value (board-char-to-square-value (aref line start))))
                          (box-on-goal-value? value))))))
    (let ((levels nil)
          (lines (load-text-lines-from-file file-name))
          (right-trim-characters (list BOARD_CHAR_FLOOR1
                                       BOARD_CHAR_FLOOR2
                                       BOARD_CHAR_FLOOR3))
          (count 0)
          board-lines
          line)
      (while (set! line (pop lines))
        (set! board-lines nil)
        (while (board-line? line)
          (push (string-right-trim right-trim-characters line)
                board-lines)
          (set! line (pop lines)))
        (if (>= (length board-lines) MIN_BOARD_HEIGHT)
            (push (make-level :index (1- (inc! count)) :name (format nil "~D" count) :board (reverse! board-lines) :notes nil :solution nil) levels)))
      (reverse! levels))))

(fn save-levels-to-file (levels file-name)
  (local-fns ((solution-to-text-lines (solution)
                (let ((result nil)
                      (moves  (moves-to-text-lines (solution-moves solution)))
                      (notes  (copy-list (solution-notes solution))))
                  (if (solution-name solution)
                      (push (solution-name solution) result))                 
                  (while moves
                    (push (pop moves) result))                 
                  (if notes
                      (do.. (push "" result)   
                            (while notes
                              (push (pop notes) result))))
                  (reverse! result))))
    (let ((lines nil)
          level)
      (while (set! level (pop levels))
        (if lines
            (push "" lines))
        (if (level-name level)
            (push (level-name level) lines))
        (dolist (line (level-board level))
          (push line lines))
        (if (level-notes level)
            (do.. (push "" lines)
                  (dolist (line (level-notes level))
                     (push line lines))))          
        (if (level-solution level)
            (do.. (push "" lines)
                  (dolist (line (solution-to-text-lines (level-solution level)))
                    (push line lines)))))
      (save-text-lines-to-file (reverse! lines) file-name))))

(fn load-level (level &optional update-text-version-of-level-board?)
  (let ((lines (level-board level))
        (width 0)
        (row   0)
        new-board)
    (set! search-level                                                    level)
    (set! timestamp-board-pool                                            nil) ; new timestamp boards with the correct dimensions are needed
    (set! (freezing-move-timestamps 0)                                    nil) ; -
    (set! pusher-reachable-squares-board-vector                           nil) ; -
    (set! (fill-pointer pusher-reachable-squares-for-search-depth-vector) 0)   ; -
    (set! reverse-mode?                                                   nil)
    (dolist (line lines)
      (maximize! width (length line)))
    (set! new-board (make-board width (length lines)))
    (dolist (line lines)
      (dotimes (col (length line))
        (set! (aref new-board (col-row-to-square col row width))
              (board-char-to-square-value (aref line col))))
      (inc! row))
    (if (load-board new-board)
        (do.. (if update-text-version-of-level-board?
                  (set! (level-board level) (board-to-text-lines new-board)))
              new-board))))

(fn get-timestamp-board-from-pool ()
  (if timestamp-board-pool 
      (pop timestamp-board-pool)
      (make-board board-width board-height)))

(fn release-timestamp-board-to-pool (timestamp-board)
  (push timestamp-board timestamp-board-pool))

(fn initialize-timestamp-board (result)
  (if (not result)
      (set! result (get-timestamp-board-from-pool)))    
  (set! (.timestamp result) 0)
  (dotimes (index (length result))
    (if (> index BOARD_INDEX_TIMESTAMP)
        (set! (aref result index) (+ MAX_TIMESTAMP 4))))
  (for-each-floor-square square
    (set! (aref result square) 0))
  result)

(fn calculate-pusher-path (from-square to-square) 
  ; the function calculates a pusher path between the two squares for the 
  ; current game state on the board. the path is optimized for moves, but
  ; not for the pusher lines secondary metric.
  (if (and (floor-square?    from-square)
           (not (box-square? from-square))
           (floor-square?    to-square)
           (not (box-square? to-square))
           (not-equal?       from-square to-square))
    (let ((result            nil)
          (queue             pusher-reachable-squares-board-vector) ; a board-sized square stack or queue, not really a board
          (queue-bottom      0)
          (queue-top         0)
          timestamp    
          square)
      (with-timestamp-board (visited)
        (if (>= (.timestamp visited) (- MAX_TIMESTAMP (length board) 10))
            (initialize-timestamp-board visited))
        (set! timestamp (inc! (.timestamp visited) 2))
        (set! (aref visited from-square) (+ timestamp from-square)) ; mark "from square" as visited
        (set! (aref queue   queue-top)   from-square)               ; put  "from square" on the queue
        (inc! queue-top)            
        (while (< queue-bottom queue-top) ; visit reachable squares using a breadth-first search   
          (set! square (aref queue queue-bottom))
          (inc! queue-bottom)        
          (for-each-neighbor-square square neighbor
            (if (and (< (aref visited  neighbor) timestamp)
                     (not (box-square? neighbor))
                     (> queue-top      0)) ; >: path to "to square" not found yet
                (do.. (set! (aref queue queue-top) neighbor)
                      (inc! queue-top) ; points to the next free element in the queue vector
                      ; register path parent square rather than distance in the "visited" vector
                      (set! (aref visited neighbor) (+ timestamp square)) 
                      (if (equal? neighbor to-square) ; true: found path. create moves on the path.
                          (do.. (set! queue-top 0) ; 0: exit enclosing "while" loop
                                (while (not-equal? from-square to-square) ; for each square on the path             
                                  (set! neighbor (- (aref visited to-square) timestamp))
                                  (for-each-direction direction
                                    (if (equal? (neighbor-square neighbor direction) to-square)
                                        (push (make-move :direction direction :box-index nil)
                                              result)))
                                  (set! to-square neighbor)))))))) ; back to previous square on the path
        (inc! (.timestamp visited) (length board))) ; make the timestamp board ready for reuse before it's recycled
      result))) ; return moves on path

(fn calculate-pusher-reachable-squares (result) 
  ; the function calculates the pusher reachable squares 
  ; for the current game state on the board, i.e., the
  ; calculation is based on the global values "pusher-square"
  ; and "board"
  (if (or (not result)
          (>= (.timestamp result) MAX_TIMESTAMP))
      (set! result (initialize-timestamp-board result)))
  (let ((timestamp         (inc! (.timestamp result) 2))
        (stack             pusher-reachable-squares-board-vector) ; a board-sized square stack or queue, not really a board
        (stack-top         1)
        (min-pusher-square pusher-square)
        square)
    (set! (aref result pusher-square) timestamp)     ; mark "pusher-square" as visited
    (set! (aref stack  stack-top    ) pusher-square) ; put  "pusher square" on the queue
    (while (> stack-top 0) ; visit reachable squares using a depth-first search   
      (set! square (aref stack stack-top))
      (dec! stack-top)
      (for-each-neighbor-square square neighbor
        (if (< (aref result neighbor) timestamp)
            (if (box-square? neighbor)
                (set! (aref  result neighbor) (1+ timestamp))
                (do.. (inc! stack-top) ; points to topmost used element in the stack vector
                      (set! (aref stack stack-top) neighbor)
                      (set! (aref result neighbor) timestamp)
                      (if   (< neighbor min-pusher-square)
                            (set! min-pusher-square neighbor)))))))
    (set! (.board-pusher-square result) min-pusher-square)  
    result))

(fn calculate-pusher-reachable-squares-for-search-depth (depth)
  (while (<= (fill-pointer pusher-reachable-squares-for-search-depth-vector) depth)
      (vector-push-extend (initialize-timestamp-board nil) pusher-reachable-squares-for-search-depth-vector))  
  (calculate-pusher-reachable-squares (pusher-reachable-squares-for-search-depth depth)))

(mc pusher-reachable-squares-calculated-for-search-depth? (depth)
  `(and (> (fill-pointer pusher-reachable-squares-for-search-depth-vector) ,depth)
        (not (zero? (normalized-pusher-square (pusher-reachable-squares-for-search-depth ,depth))))))

(mc clear-pusher-reachable-squares-for-search-depth (depth)
 `(set! (aref (pusher-reachable-squares-for-search-depth ,depth) BOARD_INDEX_PUSHER) 0))
    
(fn pusher-reachable-squares-list ()
  (let ((result nil))
    (with-timestamp-board (reachable (calculate-pusher-reachable-squares nil))
      (for-each-square square
        (if (reachable-floor? square reachable)
            (push square result))))
    (sort! result)))

(struct distance-queue-item 
  (pusher-square 0 :type fixnum) 
  (box-square    0 :type fixnum) 
  (distance      0 :type fixnum)
  next)

(fn create-distance-queue-item (pusher-square box-square distance)
  (if distance-queue-item-pool
      (let ((result distance-queue-item-pool))
        (set!  distance-queue-item-pool                  (distance-queue-item-next result))
        (set! (distance-queue-item-pusher-square result) pusher-square)
        (set! (distance-queue-item-box-square    result) box-square)
        (set! (distance-queue-item-distance      result) distance)
        (set! (distance-queue-item-next          result) nil)        
        result)
      (make-distance-queue-item :pusher-square pusher-square :box-square box-square :distance distance :next nil)))

(fn release-distance-queue-item-to-pool (queue-item)
  (set! (distance-queue-item-next queue-item) distance-queue-item-pool)
  (set!  distance-queue-item-pool queue-item))

(mc square-direction-distances (square direction distances) `(aref (aref ,distances ,direction) ,square))

(fn calculate-box-push-or-pull-distances (push? continue? start-pusher-square start-box-square result)
  (let ((original-pusher-square          pusher-square)
        (original-start-box-square-value (if start-box-square (squares start-box-square)))
        (distance-start-value            0)
        (last-box-square                 0)
        (queue-head                      nil)
        (queue-tail                      nil)
        queue-item
        box-square
        box-to-square
        pusher-from-square
        pusher-to-square
        distance
        neighbor)  
    (local-fns ((enqueue (pusher-square box-square distance)
                   (let ((result (create-distance-queue-item pusher-square box-square distance)))
                      (if queue-head
                          (set! (distance-queue-item-next queue-tail) result)
                          (set! queue-head result))
                     (set! queue-tail result)))
                 (dequeue ()
                   (if queue-head
                       (let ((result queue-head))
                         (set! queue-head (distance-queue-item-next result))
                         result)))
                 (painting-pusher-into-corner? (square)
                   (and (not-equal? square level-start-pusher-square)
                        (dead-end-square? square))))
      (with-timestamp-board (reachable)
        (if (not result)
            (do.. (set! result (make-array DIRECTION_COUNT))
                  (for-each-direction direction
                    (set! (aref result direction) (get-timestamp-board-from-pool)))))
      
        (if (and start-box-square (not (zero? start-box-square)))
            (if (box-value? original-start-box-square-value)
                (dec! (squares start-box-square) BOX))
            (set! start-box-square nil))
        
        (if continue?  ; true: not in use and not tested
            (for-each-direction direction
              (let ((distance (square-direction-distances start-box-square direction result)))
                (if (not-equal? (abs distance) DISTANCE_INFINITY)
                    (set! distance-start-value (max distance-start-value distance)))))
            (for-each-square square
              (let ((distance (if (wall-square? square) 
                                  (- DISTANCE_INFINITY) 
                                  DISTANCE_INFINITY)))
                 (for-each-direction direction
                   (set! (square-direction-distances square direction result) distance)))))
      
        (if (and (number? start-pusher-square) (not (zero? start-pusher-square)))
            (if start-box-square
                (do.. (set! pusher-square start-pusher-square)
                      (inc! (squares start-box-square) BOX)
                      (calculate-pusher-reachable-squares reachable)
                      (dec! (squares start-box-square) BOX)))
            (set!  start-pusher-square nil))
      
        ; put starting positions on the queue
        (if start-box-square ; true: not in use and not tested
            (for-each-direction direction
              (set! neighbor (neighbor-square start-box-square direction))
              (if (and (not (wall-square? neighbor))
                       (or  (not start-pusher-square)
                            (reachable-floor? neighbor reachable)))
                  (do.. (enqueue neighbor start-box-square 0)
                        (if push?
                            (set! (square-direction-distances start-box-square (opposite-direction direction) result) distance-start-value)
                            (set! (square-direction-distances start-box-square                     direction  result) distance-start-value)))
                  (if (not start-pusher-square)
                      (if push?
                          (set!    (square-direction-distances start-box-square (opposite-direction direction) result) distance-start-value)
                          (set!    (square-direction-distances start-box-square                     direction  result) distance-start-value)))))
            (if push?
                (for-each-box index
                  (let ((square (aref box-squares index)))
                    (if (not (zero? square))
                        (do.. (if start-pusher-square
                                  (do.. (set! pusher-square start-pusher-square)
                                        (inc! (squares square) BOX)
                                        (calculate-pusher-reachable-squares reachable)
                                        (dec! (squares square) BOX)))
                              (for-each-direction direction
                                (set! neighbor (neighbor-square square direction))
                                (if (and (not (wall-square? neighbor))
                                         (or  (not start-pusher-square)
                                              (reachable-floor? neighbor reachable)))
                                    (do.. (enqueue neighbor square 0)
                                          (set! (square-direction-distances square (opposite-direction direction) result) 0))
                                    (if (not start-pusher-square)
                                        (set! (square-direction-distances square direction result) 0))))))))
                (for-each-goal index
                  (let ((square (aref goal-squares index)))
                    (if (goal-square? square)
                        (for-each-direction direction
                          (set! (square-direction-distances square direction result) 0)
                          (set! neighbor   (neighbor-square square direction))
                          (if (not (wall-square? neighbor))                                             
                              (enqueue neighbor square 0))))))))
                
        ; visit reachable squares using a breadth-first search   
        (while (set! queue-item (dequeue))
          (set! pusher-square     (distance-queue-item-pusher-square queue-item))
          (set! box-square        (distance-queue-item-box-square    queue-item))
          (set! distance      (1+ (distance-queue-item-distance      queue-item)))
          (release-distance-queue-item-to-pool                       queue-item)
        
          (inc! (squares box-square) BOX)
        
          (if (or (not-equal? box-square last-box-square)
                  (not (reachable-floor? pusher-square reachable)))
              (do.. (calculate-pusher-reachable-squares reachable)
                    (set! last-box-square box-square)))
                
          (for-each-direction direction
            (set! box-to-square (neighbor-square box-square direction))
            (if push?
                (do.. (set! pusher-from-square (neighbor-square box-square (opposite-direction direction)))
                       (if   (and (< distance (square-direction-distances box-to-square direction result))
                                 (reachable-floor? pusher-from-square reachable)
                                 (zero? (bitand (squares box-to-square) WALL+BOX+ILLEGAL_BOX_SQUARE)))
                             (do.. (set! (square-direction-distances box-to-square direction result) distance)
                                   (enqueue box-square box-to-square distance))))
                (do.. (set! pusher-to-square (neighbor-square box-to-square direction))
                      (if   (and (< distance (square-direction-distances box-to-square direction result))
                                 (reachable-floor? box-to-square reachable)
                                 (floor-square? pusher-to-square)
                                 (zero? (bitand (squares box-to-square) WALL+BOX+ILLEGAL_BOX_SQUARE))
                                 (not (painting-pusher-into-corner? pusher-to-square))) 
                            (do.. (set! (square-direction-distances box-to-square direction result) distance)
                                  (enqueue pusher-to-square box-to-square distance))))))                           
        
          (dec! (squares box-square) BOX)))
      
      (set! pusher-square              original-pusher-square)
      (if start-box-square
          (set! (squares start-box-square) original-start-box-square-value))
      result)))

(fn calculate-nearest-start-or-goal-distances (start-distances? mark-unreachable-squares? start-pusher-square)
  (remove-boxes-from-board)
  (let ((result    (get-timestamp-board-from-pool))
        (distances (calculate-box-push-or-pull-distances start-distances? nil start-pusher-square nil nil)))  
    (put-boxes-on-board)
    (for-each-square square
      (set! (aref result square) DISTANCE_INFINITY)
      (if (not (wall-square? square))
          (for-each-direction direction
            (minimize! (aref result square)
                       (square-direction-distances square direction distances))))
      (if (and (equal? (aref result square) DISTANCE_INFINITY)
               (floor-square? square)
               mark-unreachable-squares?)
          (set-bit! (if start-distances? UNREACHABLE_BOX_SQUARE_BIT ILLEGAL_BOX_SQUARE_BIT) (squares square))))
    ;(if start-distances? (display "nearest start distances") (display "nearest goal distances"))
    ;(display-board result 0) (nl)
    result))

(struct transposition-table-item
  (hash-value             0 :type integer)
  (pusher-square          0 :type fixnum)
  (box-index              0 :type fixnum)
  (move-direction         0 :type fixnum)
  (push-count             0 :type fixnum)
  (score                  0 :type fixnum)
  (successor-count        0 :type fixnum)
  (best-forgotten-score   0 :type fixnum)
  (flags                  0 :type fixnum)
  parent
  links-prev              ; links for an open list or a perimeter node list
  links-next
  hash-value-next         ; next item with same hash value. the items are on a circular list.
  next)                   ; next item on the linked list with all items, both free and in use

(mc .hash-value           (position) `(transposition-table-item-hash-value           ,position))
(mc .pusher-square        (position) `(transposition-table-item-pusher-square        ,position))
(mc .box-index            (position) `(transposition-table-item-box-index            ,position))
(mc .move-direction       (position) `(transposition-table-item-move-direction       ,position))
(mc .push-count           (position) `(transposition-table-item-push-count           ,position))
(mc .score                (position) `(transposition-table-item-score                ,position))
(mc .successor-count      (position) `(transposition-table-item-successor-count      ,position))
(mc .best-forgotten-score (position) `(transposition-table-item-best-forgotten-score ,position))
(mc .flags                (position) `(transposition-table-item-flags                ,position))
(mc .parent               (position) `(transposition-table-item-parent               ,position))
(mc .links-prev           (position) `(transposition-table-item-links-prev           ,position))
(mc .links-next           (position) `(transposition-table-item-links-next           ,position))
(mc .hash-value-next      (position) `(transposition-table-item-hash-value-next      ,position))
(mc .next                 (position) `(transposition-table-item-next                 ,position))
(mc .data                 (position) `(transposition-table-item-data                 ,position))
(mc .hash-value-previous  (position) `(transposition-table-item-hash-value-previous  ,position))

(fn transposition-table-item-data (position) ; data for display
  (list (.hash-value position) (bit? POSITION_FLAG_HASH_TABLE_BIT (.flags position))
        (square-index-to-text  (.pusher-square position)) 
        (.box-index position)  (direction-to-char (.move-direction position)) 
        (.push-count position) (.score position)))

(mc for-each-transposition-table-item (item &rest body)
  ; iterates over all items in the transposition table, 
  ; and executes the specified forms for each item. the return value is undefined.
  (let ((next  (new-symbol)))
    `(let ((,item transposition-table-items)
           ,next)
       (while  ,item
         (set! ,next (.next ,item))         ; remember the next item
         (if   (not (position-free? ,item)) ; true: the item is in the transposition table, not on the free list
               (do.. ,@body))
         (set! ,item ,next)))))             ; advance to the next item, if any

(mc add-transposition-table-item-to-front-of-list (item list)
  `(do.. (if ,list
             (do.. (set! (.links-prev ,item) 
                         (.links-prev ,list))
                   (set! (.links-next ,item) 
                         ,list)          
                   (set! (.links-next (.links-prev ,item))               
                         ,item)
                   (set! (.links-prev (.links-next ,item))               
                         ,item))
             (do.. (set! (.links-prev ,item) ,item) ; circular list with item as only member
                   (set! (.links-next ,item) ,item)))
         (set! ,list ,item)))

(mc remove-transposition-table-item-from-list (item list)
  (let ((prev (new-symbol))
        (next (new-symbol)))
    `(let ((,prev        (.links-prev ,item))
           (,next        (.links-next ,item)))
       (set! (.links-next ,prev)      ,next)
       (set! (.links-prev ,next)      ,prev)    
       (set! (.links-prev ,item)       nil)           
       (set! (.links-next ,item)       nil)                  
       (if (same-object? ,item ,list)
           (set! ,list (if (same-object? ,item ,next) ; true: removing last item from list
                           nil
                           ,next)))
       ,item)))

(fn get-transposition-table-item-from-pool () 
  (if transposition-table-item-pool
      (let ((result transposition-table-item-pool))
        (dec! (.flags result)               POSITION_FLAG_FREE)
        (set! transposition-table-item-pool (.links-next result))
        result)))

(fn release-transposition-table-item-to-pool (item)
  (inc! (.flags      item) POSITION_FLAG_FREE)
  (set! (.links-next item) transposition-table-item-pool)
  (set! transposition-table-item-pool item))

(fn create-transposition-table-item (hash-value pusher-square box-index move-direction push-count score 
                                     flags parent)
  (if transposition-table-item-pool
      (let ((result transposition-table-item-pool))
        (set!  transposition-table-item-pool (.links-next result))
        (set! (.hash-value           result) hash-value)
        (set! (.pusher-square        result) pusher-square)
        (set! (.box-index            result) box-index)
        (set! (.move-direction       result) move-direction)
        (set! (.push-count           result) push-count)
        (set! (.score                result) score)
        (set! (.successor-count      result) 0)
        (set! (.best-forgotten-score result) DEAD_END_SCORE)            
        (set! (.flags                result) flags)
        (set! (.parent               result) parent)
        (set! (.links-prev           result) result)
        (set! (.links-next           result) result)
        (set! (.hash-value-next      result) result)        
        result)
      (let ((result (make-transposition-table-item
                       :hash-value                                   hash-value
                       :pusher-square                                pusher-square 
                       :box-index                                    box-index
                       :move-direction                               move-direction
                       :push-count                                   push-count
                       :score                                        score
                       :successor-count                              0
                       :best-forgotten-score                         DEAD_END_SCORE
                       :flags                                        flags
                       :parent                                       parent
                       :links-prev                                   nil
                       :links-next                                   nil
                       :hash-value-next                              nil
                       :next                                         transposition-table-items)))
        (set! (.links-prev           result) result) ; make circular list with a single item
        (set! (.links-next           result) result)
        (set! (.hash-value-next      result) result) ; make circular list with a single item       
        (set! transposition-table-items      result) ; linked list with all positions, both free and in use
        result)))

(fn transposition-table-initialize ()
  ; release all transposition table items to the free items pool
  (if (not (zero? transposition-table-item-count)) ; otherwise all existing positions, if any, are already on the free list  
      (for-each-transposition-table-item position
        (release-transposition-table-item-to-pool position)))
  (set! transposition-table-item-count 0)
  (hash-table-clear! transposition-table))

(fn transposition-table-finalize ()
  (transposition-table-initialize))

(fn transposition-table-item-hash-value-previous (position) 
  ; returns the previous transposition table item with the same hash key, if any. 
  ; otherwise the position itself is returned.
  (let ((result position))
    (while (not (same-object? position (.hash-value-next result))) ; same-hash-value positions are on a circular list
           (set! result                (.hash-value-next result)))
    result))
       
(fn transposition-table-lookup (hash-value pusher-square search-depth)
  ; preconditions: if "search-depth" is non-nil, then it must be one of the search depths in 
  ; progress, and the specified hash value and pusher square must match the current state 
  ; of the game board. in that case, the lookup doesn't just search for an exact match, but 
  ; for any position with the pusher in the current pusher access area on the board. to that
  ; end, there is one more precondition: the calculation of the pusher reachable squares for 
  ; the given search depth must have been properly initialized, either by being already 
  ; calculated, or by being marked as not yet calculated. 
  (let ((result nil)
        (position (hash-table-lookup hash-value transposition-table)))
    (set! transposition-table-lookup-first-item-with-hash-value position) ; remember "same-hash-value" chain root
    (while (and (not result) position) ; until found, or until no more items to check with an identical hash value
      (if (equal? pusher-square (.pusher-square position)) ; true: found matching item
          (set! result position) ; return the matching position
          (do.. (if search-depth ; true: search for a position with the pusher square in the same access area as the pusher on the game board
                    (do.. (if (not (pusher-reachable-squares-calculated-for-search-depth? search-depth))
                               (calculate-pusher-reachable-squares-for-search-depth       search-depth))
                          (if (reachable-floor? (.pusher-square position) ; true: pusher from "position" in current access area
                                                (pusher-reachable-squares-for-search-depth search-depth)) 
                              (set! result position)))) 
                (set! position (.hash-value-next position)) ; advance to the next item with the same hash value, if any
                (if (same-object? position transposition-table-lookup-first-item-with-hash-value) ; true: end of list
                    (set! position nil))))) ; nil: exit loop
    result))

(fn transposition-table-add (hash-value pusher-square box-index move-direction push-count score parent search-depth)
  ; preconditions: if the search depth is non-nil, then the specified game state values must match the current game
  ; state on the board, and the pusher reachable squares for the given search depth must have been properly initialized,
  ; either by being already calculated, or by being marked as not yet calculated.
  ; side effect: if the specified game state matches a perimeter node (a leaf position from a search in the opposite
  ; direction), then the simple lower bound is set to zero, as if the current game state is a solution.
  (let ((result (transposition-table-lookup hash-value pusher-square search-depth)))
    (if result ; true: the position already exists in the transposition table
        (if (or (<= (.push-count                         result) push-count)      ; <= : the position has been reached earlier with fewer or the same number of pushes
                (>= (.score                              result) DEAD_END_SCORE)  ; >= : the position is a dead end or a deadlock
                (<= (.pusher-square                      result) 0))              ; <= : special items, e.g., deadlock patterns
            (do.. (inc! search-count-duplicate-positions)
                  (set! transposition-table-add-result-type 'duplicate))
            ; new better path to an already stored position
            (do.. (if (position-on-perimeter? result)    ; true: it's a leaf position from a search in the opposite direction
                      (do.. (set! simple-lower-bound     0) ; 0: fake that this position is a solution
                            (set! (.score result)        (+ push-count (.best-forgotten-score result))) ; the solution path length in pushes via this position 
                            ;(dec! (.flags               result) POSITION_FLAG_PERIMETER) 
                            ;(display "found perimeter node" (.data result) "remaining pushes" (.best-forgotten-score result)
                            ;         "pushes" push-count "score" score) 
                            ;(display-board)
                            ;(read-line)
                            (if search-perimeter-list ; otherwise, there is no perimeter list, and the position is just flagged as a target position.
                                (remove-transposition-table-item-from-list result search-perimeter-list)))
                      (do.. (if (position-open?          result) 
                                ; remove the position from the open queue. the caller puts 
                                ; the position back on the queue at its new proper position.
                                (open-queue-remove result))
                            (transposition-table-update-parent-when-successor-is-deleted result) ; update old parent, if any
                            ;(display "new best path" (.data result) "new score" score)
                            (set! (.score                result) score))) ; save the new score for the position
              
                  (if   (bit? POSITION_FLAG_SOLUTION_PATH_BIT (.flags result)) ; true: new best path to a position on the best found solution path
                        (do.. (update-path-flags (.parent result) (- POSITION_FLAG_SOLUTION_PATH))  ; remove old best path flags
                              (update-path-flags parent              POSITION_FLAG_SOLUTION_PATH))) ; add    new best path flags
                  (set! (.hash-value                     result) hash-value)              
                  (set! (.pusher-square                  result) pusher-square)
                  (set! (.box-index                      result) box-index)
                  (set! (.move-direction                 result) move-direction)
                  (set! (.push-count                     result) push-count)
                  (set! (.parent                         result) parent)    
                  (if   parent
                        (inc! (.successor-count          parent)))            
                  (inc! search-count-new-best-paths)
                  (set! transposition-table-add-result-type 'new-best-path)))
        ; the position was not found in the transposition table. try to add it now.     
        (do.. (if parent ; protect the parent against pruning by updating its successor count up front
                  (inc! (.successor-count parent)))
              (if (< transposition-table-item-count transposition-table-size-limit)
                  (set! result t) ; t: create a new transposition table item further down
                  ; the transposition table is full. check if nodes can be recycled.
                  (if search-perimeter-list ; true: take a transposition table item from the perimeter list and reuse it
                      (do.. (set! result                search-perimeter-list) 
                            (inc! search-count-recycled-positions)
                            (remove-transposition-table-item-from-list search-perimeter-list search-perimeter-list)
                            (transposition-table-remove result))
                      (if (and search-node-recycling-enabled?
                               (set! result (open-queue-worst-position-eligible-for-deletion)))
                          (if t
                              ;(or (< score             (.score result))
                              ;    (and (equal? score   (.score result))
                              ;         (> push-count   (.push-count result)))) ; >: closer to a solution                              
                              (do.. (inc! search-count-recycled-positions) 
                                    (open-queue-remove          result)
                                    (transposition-table-remove result))
                              (set! result nil)))))
              (if result 
                  ; create and add the new position to the transposition table
                  (do.. (set! result (create-transposition-table-item 
                                        hash-value pusher-square box-index move-direction 
                                        push-count score 0 parent))
                        (if transposition-table-lookup-first-item-with-hash-value
                            ; add the new item to the existing list of items with identical hash values
                            (do.. (set! (.hash-value-next result)  (.hash-value-next transposition-table-lookup-first-item-with-hash-value))
                                  (set! (.hash-value-next transposition-table-lookup-first-item-with-hash-value) result))
                            ; add the new item to the hash table, creating a single-item circular list for this hash value
                            (do.. (inc! (.flags result) POSITION_FLAG_HASH_TABLE)
                                  (hash-table-add! hash-value result transposition-table)))
                        (inc! transposition-table-item-count)
                        (if (> pusher-square 0)
                            (inc! search-count-positions))
                        (set! transposition-table-add-result-type 'new))
                  ; don't add the new position to the transposition table                  
                  (do.. (if parent
                            (do.. (dec!      (.successor-count      parent)) ; undo the premature successor count updating
                                  (minimize! (.best-forgotten-score parent) score))) ; update the best pruned score for the parent position
                        (minimize! search-best-pruned-score score) ; update the best pruned score during the search
                        (set! transposition-table-add-result-type 'pruned)))))
    result))

(fn transposition-table-update-parent-when-successor-is-deleted (successor) 
  ; updates the parent position when the successor is deleted from the transposition table. 
  (let ((parent (.parent successor)))
    (if parent
        (do.. (dec!      (.successor-count                 parent))
              (minimize! (.best-forgotten-score            parent)                
                         (.score                           successor))          
              (if (and   (zero? (.successor-count          parent))
                         (not (position-on-path?           parent))
                         (not (position-open?              parent)))
                  (do..  ; put parent back on open queue, unless it's a dead end or a deadlock
                         (set! (.score parent)             (.best-forgotten-score parent))
                         (if (< (.score parent)            DEAD_END_SCORE)
                             (open-queue-add               parent))))))))

(fn transposition-table-remove (position)      
  (transposition-table-update-parent-when-successor-is-deleted position) ; update the parent of this position, if any
  ; remove the position from the circular list of items having the same hash value
  (set! (.hash-value-next (.hash-value-previous position)) (.hash-value-next position)) 
  (dec! transposition-table-item-count) 
  (if (bit? POSITION_FLAG_HASH_TABLE_BIT (.flags position))
      ; remove the position from the table. if there are more positions
      ; with the same hash value, then add a new representative to the table.
      (do.. (hash-table-remove! (.hash-value position) transposition-table)
            (if (not (same-object? position (.hash-value-next position))) ; true: more positions with the same hash value
                (do.. (inc! (.flags (.hash-value-next position)) POSITION_FLAG_HASH_TABLE)
                      (hash-table-add! (.hash-value (.hash-value-next position)) (.hash-value-next position) transposition-table)))))
  (release-transposition-table-item-to-pool position))

(fn display-transposition-table-item (position)
  (display (.data position)))

(fn display-transposition-table (&optional (count (min 100 transposition-table-item-count)))
  (for-each-transposition-table-item position
    (if (> count 0)
        (display-transposition-table-item position))
    (dec! count))
  (display "count" transposition-table-item-count))

(fn open-queue-initialize ()
  (set! open-count                           0)
  (set! (fill-pointer                        open-buckets-vector) 0)
  (vector-push-extend nil                    open-buckets-vector) ; initialize the zero-value bucket on the open queue
  (set! open-min-value                       INFINITY_SCORE)
  (set! open-max-value                       0) ; must be 0 here. negative values have special meanings. "open-max-value" is also the solver status.
  (set! search-count-dequeued-open-positions 0)    
  (set! search-no-progress-rover             0))

(fn open-queue-add (position)
  (let ((score      (.score position)))
    (if (< score    open-min-value) 
        (set!       open-min-value score))
    (if (> score    open-max-value)
        (do.. ; if the search has terminated, or if a solution has been found, 
              ; then the existing queue maximum value represents an upper
              ; bound for the search, and it must not be raised to the score value
              (if (and (> open-max-value TERMINATE_SEARCH_SCORE)
                       (not search-solution-position))
                  (set! open-max-value score)) ; update the maximum value on the queue
              (if (< score UNDEFINED_SCORE)
                  ; ensure that the open queue bucket for this score has been initialized
                  (while (<= (fill-pointer  open-buckets-vector) score)
                    (vector-push-extend nil open-buckets-vector))
                  (error TEXT_INTERNAL_ERROR 2 (.data position)))))
    (if (<= score open-max-value)
        (do.. (add-transposition-table-item-to-front-of-list position (open-buckets score))
              (inc! open-count)   
              (inc! (.flags position) POSITION_FLAG_OPEN)))))

(fn open-queue-remove (position)
  (remove-transposition-table-item-from-list position (open-buckets (.score position)))
  (dec! open-count)   
  (set! (.best-forgotten-score position) DEAD_END_SCORE)  ; initialization for a new expansion of this position   
  (dec! (.flags                position) POSITION_FLAG_OPEN))

(fn open-queue-select-position-for-expansion (occasionally-pick-a-less-promising-position-as-an-attempt-to-escape-from-a-futile-line-of-play?)
  (let ((result nil))
    (while (and (<= open-min-value open-max-value) 
                (not (set! result (open-buckets open-min-value))))
       (inc! open-min-value))
    (if result  
        (do.. (inc! search-count-dequeued-open-positions)
              (if (and occasionally-pick-a-less-promising-position-as-an-attempt-to-escape-from-a-futile-line-of-play?
                       (zero? (bitand search-count-dequeued-open-positions NO_SEARCH_PROGRESS_PUSH_COUNT_BIT_MASK)))
                  (do.. (inc! search-no-progress-rover)
                        (while (and (<= search-no-progress-rover open-max-value)
                                    (open-bucket-empty? search-no-progress-rover))
                          (inc! search-no-progress-rover))  
                        (if (<= search-no-progress-rover open-max-value)         
                            (set! result (open-buckets search-no-progress-rover))  
                            (set! search-no-progress-rover open-min-value))))))
    result))

(fn open-queue-worst-position-eligible-for-deletion ()
  (let ((result nil)
        (index open-max-value)
        bucket-root)
    (while (and (not result)
                (>= index open-min-value))
      (set! result (open-buckets index))
      (if result ; true: non-empty bucket
          (do.. (set! bucket-root result)
                (set! result (.links-prev result)) ; most shallow item in bucket
                (while (and (or (not (zero? (.successor-count result)))
                                (position-on-path-or-solution-path? result))
                            (not (same-object? result bucket-root)))
                  (set! result (.links-prev result)))
                (if (or (not (zero? (.successor-count       result)))
                        (position-on-path-or-solution-path? result))
                    (do.. (set! result nil) ; back to previous score bucket                  
                          (dec! index))))
          (dec! index))) ; back to previous score bucket
    result))

(fn display-open-queue ()
  (let ((index open-min-value)) 
    (while (<= index open-max-value) ; for each score bucket
      (let ((item (open-buckets index)))
        (if item
            (if (same-object? item (.links-next item)) ; true: only one item in this bucket
                (display index ":" (.data item))
                (while (and item (not (same-object? item (open-buckets index))))
                  (display index ":" (.data item))
                  (set! item (.links-next item)))))))))

(fn reverse-path! (position) ; reverses the path to the position, using the "parent" field for the position links
  (let ((result nil)
        (last   position)
        parent)
    (while position
      (set! parent             (.parent position))
      (set! (.parent position) result)
      (set! result             position)
      (set! position           parent)
      (if (same-object? position last)
          (set! position nil)))
    result))

(fn path (position) ; returns the path to the position, with the position first and the root position last on the list
  (let ((result nil)
        (last   position))
    (while position
      (push position result)
      (set! position (.parent position))
      (if (same-object? position last)
          (set! position nil)))
    (reverse! result)))

(fn path-length (position)
  (let ((result 0)
        (last   position))
    (while position
      (inc! result)
      (set! position (.parent position))
      (if (same-object? position last)
          (set! position nil)))
    result))

(fn better-score? (new-push-count new-score old-push-count old-score)
  (or (<           new-score      old-score)
      (and (equal? new-score      old-score)
           (<      new-push-count old-push-count))))

(fn better-position-score? (new-position old-position)
  (or (not old-position) 
      (better-score? (.push-count new-position) (.score new-position)
                     (.push-count old-position) (.score old-position))))

(fn terminate-search (&optional (reason TERMINATE_SEARCH_SCORE))
  (if (or (>=     open-max-value 0)                   ; true: the search hasn't already been terminated
          (equal? reason         INVALID_PATH_SCORE)) ; true: "invalid path" overrules all other reasons for termination
      (set! open-max-value reason)                    ; register the reason for the termination
      open-max-value))                                ; return the original reason for termination

(mc search-terminated? ()
  `(< open-max-value 0))

(mc time-limit-exceeded? () ; side effect: terminates the search if the time limit is exceeded
  `(and search-limit-time-ticks
        (>= (elapsed-time-ticks search-start-time-tick-count) search-limit-time-ticks)
        (terminate-search TIME_LIMIT_EXCEEDED_SCORE)))

(mc do-push (box-index direction)
  `(if (>= ,box-index 0)  
       (do.. (move-pusher (boxes ,box-index))
             (move-box    ,box-index (neighbor-square (boxes ,box-index) ,direction)))))

(mc do-pull (box-index direction)
  `(if (>= ,box-index 0)  
       (do.. (move-box    ,box-index (neighbor-square (boxes ,box-index) ,direction))
             (move-pusher            (neighbor-square (boxes ,box-index) ,direction)))))

(mc undo-push (box-index direction)
  `(do-pull ,box-index (opposite-direction ,direction)))

(mc undo-pull (box-index direction)
  `(do-push ,box-index (opposite-direction ,direction)))

(fn backward-search-repair-box-index-and-direction? (position) ; local function for "put-position-on-the-board". global to ensure efficiency.
  (let* ((count                  0)
         (position-hash-value    (.hash-value     position))
         (position-pusher-square (.pusher-square  position))  
         box-index     
         box-square
         neighbor)
    (for-each-direction direction
      (set! neighbor (neighbor-square position-pusher-square direction))
      (if (and (floor-square? neighbor)
               (box-square?   (set! box-square (neighbor-square neighbor direction))))
          (do.. (set! box-index (box-at-square box-square))
                (set! board-hash-value   (bitxor board-hash-value (aref zobrist-bit-strings box-square)))    ; out
                (set! board-hash-value   (bitxor board-hash-value (aref zobrist-bit-strings neighbor)))      ; in 
                (if (equal? board-hash-value position-hash-value) ; true: a pull gives the position hash value
                    (do.. (set!        (.box-index      position) box-index)
                          (set!        (.move-direction position) (opposite-direction direction))
                          (inc!        count)))            
                (set! board-hash-value   (bitxor board-hash-value (aref zobrist-bit-strings neighbor)))      ; out
                (set! board-hash-value   (bitxor board-hash-value (aref zobrist-bit-strings box-square)))))) ; in           
    (if (equal? count 1)
        (inc! search-count-path-repairs)
        (display TEXT_REPAIR_BOX_AND_DIRECTION_FAILED (.data position)))
    (equal? count 1))) ; "1": found exactly one pull direction which matches the position

(fn forward-search-repair-box-index-and-direction? (position) ; local function for "put-position-on-the-board". global to ensure efficiency.
  (let* ((count               0)
         (position-hash-value (.hash-value     position))
         (box-index           (box-at-square   (.pusher-square  position)))                 
         box-square
         neighbor)
    (if box-index ; true: there is a box at the pusher-square, as there should be before the push
        (do.. (set! box-square         (boxes box-index))
              (set! board-hash-value   (bitxor board-hash-value (aref zobrist-bit-strings box-square)))
              (for-each-direction direction
                (set! neighbor         (neighbor-square box-square direction))
                (set! board-hash-value (bitxor board-hash-value (aref zobrist-bit-strings neighbor)))
                (if (equal? board-hash-value position-hash-value) ; true: a push gives the correct hash value
                    (do.. (set!        (.box-index      position) box-index)
                          (set!        (.move-direction position) direction)
                          (inc!        count)))
                (set! board-hash-value (bitxor board-hash-value (aref zobrist-bit-strings neighbor))))
              (set!   board-hash-value (bitxor board-hash-value (aref zobrist-bit-strings box-square)))))
    (if (equal? count 1)
        (inc! search-count-path-repairs)
        (display TEXT_REPAIR_BOX_AND_DIRECTION_FAILED (.data position)))
    (equal? count 1))) ; "1": found exactly one push direction which matches the position
 
(fn put-position-on-the-board (position)
  (let ((p    position)
        (next nil)
        parent
        position-pusher-square
        box-index
        box-square
        direction) 
   
    ; find the path back to the common ancestor of the
    ; new position and the current position on the board
    (while (and p       (not (position-on-path? p)))
      (set! parent      (.parent p))
      (set! (.parent p) next) ; pointer-reversal, temporarily using the "parent" field for forward links
      (set! next        p)
      (set! p           parent))

    ; undo moves on the current path back to the common ancestor
    ; of the current position and the new position
    (while (and current-position (not (same-object? current-position p))) 
      (if reverse-mode?
          (undo-pull (.box-index current-position) (.move-direction current-position))
          (undo-push (.box-index current-position) (.move-direction current-position)))          
      (dec! (.flags current-position) POSITION_FLAG_PATH)      
      (set! current-position          (.parent current-position)))
    
    ; perform the moves on the new path, starting from the successor 
    ; of the common ancestor of the old path and the new path
    (if next
        (while next ; for each position along the new part of the path to "position"

          (if reverse-mode?
              (if (zero? search-count-recycled-positions) ; true: no risk of path changes
                  (do-pull (.box-index      next)
                           (.move-direction next))            
                  (do..
                    (set! box-index        (.box-index       next))                 
                    (set! direction        (.move-direction  next))                   
                    (if   (>= box-index 0) 
                          (do.. (set! position-pusher-square (.pusher-square   next))  
                                (set! box-square             (boxes            box-index))                            
                                (if   (not-equal? position-pusher-square ; true: invalid pull, i.e., the path must have changed
                                                  (+ box-square (* 2 (aref neighbor-square-offsets direction))))
                                      (if (backward-search-repair-box-index-and-direction? next)  
                                          (do.. (set! box-index   (.box-index       next))
                                                (set! direction   (.move-direction  next)))
                                          (do.. (set! next        nil) ; exit loop
                                                (terminate-search INVALID_PATH_SCORE))))))
                    (do-pull box-index direction)))
              (do.. ; the forward search is - in contrast to the backward search - allowed to use an 
                    ; evaluation function which doesn't make the search an A* search. the path 
                    ; to a position may change during the search. since position identity checking 
                    ; is based on hash key identity only, there is a small risk that hash key collisions 
                    ; corrupt the paths. the search must check that each move along the path is legal, 
                    ; terminating gracefully if the path is illegal.
                    ;
                    ; a more common problem than hash key collisions is that if a path  to a position
                    ; has changed, then the move which leads to the position may involve a different box,
                    ; and/or the move direction may be different. the search repairs the box index and 
                    ; the move direction in that case.
               
                    (set! box-index     (.box-index     next))                                
                    (if   (>= box-index 0)                
                          (do.. (set! position-pusher-square (.pusher-square next))  
                                (set! box-square             (boxes box-index))             
                                (if (box-square? position-pusher-square) ; true: there is a box at the "pusher-square-after-the-push", as there should be
                                    (do.. (if (not-equal? position-pusher-square box-square)
                                              (if (forward-search-repair-box-index-and-direction? next)
                                                  (set! box-index (.box-index next))
                                                  (do.. (set! next        nil) ; exit loop
                                                        (terminate-search INVALID_PATH_SCORE))))
                                          ;(set! (.board-pusher-square (pusher-reachable-squares-for-search-depth (.push-count next))) 
                                          ;      0) ; 0: not calculated yet
                                          (do-push box-index (.move-direction next)))
                                    ; something is wrong, such as a hash value collision having corrupted the paths                     
                                    (do.. (set! next        nil) ; exit loop
                                          (terminate-search INVALID_PATH_SCORE)))))))
          (if next ; true: the path to this position is still valid
              (do.. (set-bit! POSITION_FLAG_PATH_BIT (.flags next))    ; now on current path  
                    (set! parent                     (.parent next))   ; because of the pointer-reversal, this is really the next position on the path
                    (set! (.parent next)             current-position) ; undo the pointer-reversal
                    (set! current-position           next)             ; update the current position
                    (set! next                       parent))))        ; advance to the next position on the path, if any
        ; no new box moves, but the pusher square on the board must be updated     
        (do.. (if position
                  (do.. (move-pusher                     (.pusher-square position))
                        (set-bit! POSITION_FLAG_PATH_BIT (.flags         position))))
              (set! current-position position)))))
  
(fn put-solution-position-on-the-board () ; puts the solution position on the board, checking its path is intact
  (if (and search-solution-position
           (not-equal? open-max-value INVALID_PATH_SCORE)) 
      (do.. ; to ensure that the full solution path is put on the board also when the 
            ; start position and the end position are identical, it's necessary first
            ; to put the next to last position on the board
            (put-position-on-the-board (.parent search-solution-position))
            (put-position-on-the-board          search-solution-position)))
  (if (equal? open-max-value INVALID_PATH_SCORE) 
      ; a corrupted path has been found during the search, e.g., as the result of
      ; a hash key collision. destroy the solution, if any. 
      (set! search-solution-position nil))
  search-solution-position) ; return the solution position, if any

(fn display-status (&optional message (transposition-table-item-count transposition-table-item-count))
  (let ((time (elapsed-time-ticks search-start-time-tick-count)))
    (set! search-time-milliseconds            (tick-count-to-milliseconds time))    
    (set! display-status-next-time-tick-count (+ time display-status-time-interval-tick-count))
    (if message
        (display message))
    (displayln (format nil TEXT_FORMAT_DISPLAY_STATUS 
                           (1+ (level-index search-level)) (round search-time-milliseconds 1000)
                           transposition-table-item-count open-count search-count-pushes))
    (if (not (zero? search-count-recycled-positions))
        (display TEXT_RECYCLED_POSITIONS search-count-recycled-positions))        
    (if (search-terminated?)
        (displayln (search-state-to-text open-max-value)))))

(fn update-path-flags (position value)
  (let ((last position))
    (while position
      (inc! (.flags position) value)
      (set! position (.parent position))
      (if (same-object? position last) ; true: seeing the original position for the second time
          (set! position nil)))))

(fn save-perimeter-positions ()
  ; save all transposition table leaf positions (perimeter positions) before a search in the 
  ; opposite direction. all non-leaf positions are recycled.
  (let ((count 0)
        (time-check-countdown-mask (1- (bit-shift-arithmetic 1 12))) ; a 2^N - 1 number
        perimeter-list-last
        bucket
        bucket-last)
    (set! search-perimeter-list nil)
    ; use the open queue to sort the perimeter nodes on estimated distances to the target position
    (open-queue-initialize) 
    ; positions with no successors are leaf nodes (perimeter nodes)
    (for-each-transposition-table-item position
      (set!         (.parent position)              nil) ; nil: don't update a parent during deletion
      (if (and (not (search-terminated?))
               (or  (bitand (inc! count)            time-check-countdown-mask) ; bitand: only perform time checks periodically
                    (not    (time-limit-exceeded?))))
          (if (and  (zero?  (.successor-count       position))
                    (<      (.score                 position) UNDEFINED_SCORE)
                    (>      (.pusher-square         position) 0)) ; > 0: not a special position, such as a deadlock pattern
              ; put the leaf position on the perimeter list, initializing the leaf position to a perimeter node
              (do.. ; use the estimated distance to the target position as score rather than the estimated solution path score.
                    ; that way, the perimeter nodes which are believed to be closer to the target position, will be recycled last.
                    (if A*-search?
                        (dec! (.score position)     (min (.push-count position) (.score position)))) 
                    (clear-bit! POSITION_FLAG_OPEN_BIT (.flags position)) ; clear old open flag, if any
                    (open-queue-add                 position) ; sort leaf nodes on score, using the open queue
                    (set!   (.best-forgotten-score  position) (.push-count position)) ; save number of pushes from position to solved state
                    (set!   (.push-count            position) UNDEFINED_SCORE) ; allow the search in the opposite direction to find a less costly path
                    (set!   (.flags                 position) (bitor POSITION_FLAG_PERIMETER (bitand (.flags position) POSITION_FLAG_HASH_TABLE))))
              ; a special position, an interior position, a deadlock leaf position, or a dead end leaf position. delete it.
              (do.. (transposition-table-remove     position)))))
    (if (not (search-terminated?))
        ; put all leaf nodes on the perimeter list, sorted in descending order on score
        (dotimes (index (1+ open-max-value))
          (if (set! bucket (open-buckets index))
              (do.. (if search-perimeter-list ; true: not the first non-empty bucket
                        (do.. ; add items in the bucket to the existing perimeter list, keeping it a circular list
                              (set! perimeter-list-last                 (.links-prev search-perimeter-list))
                              (set! bucket-last                         (.links-prev bucket))
                              (set! (.links-next bucket-last)           search-perimeter-list)
                              (set! (.links-prev search-perimeter-list) bucket-last)
                              (set! (.links-next perimeter-list-last)   bucket) 
                              (set! (.links-prev bucket)                perimeter-list-last)))
                    (set!     search-perimeter-list                     bucket))))) ; puts items in this bucket at the front of the list
    ;(open-queue-initialize))) ; unnecessary. the next search initializes the queue.
    ;(display "perimeter list")
    ;(display-transposition-table)
    ;(read-line)
  ))

(fn perform-search-expand-position (position search-depth)
  ; generates successor positions for the given position. 
  ; precondition: pusher reachable squares for this search depth have been calculated.
  (let* ((result                  DEAD_END_SCORE)
         (position-push-count     (.push-count     position))
         (position-box-index      (.box-index      position))
         (position-move-direction (.move-direction position))
         (position-box-square     (if (>= position-box-index 0) (boxes position-box-index) -1))
         (position-score          (.score position))
         (successor-push-count    (1+ position-push-count))
         (successor-depth         (1+ search-depth))
         (reachable               (pusher-reachable-squares-for-search-depth search-depth))
         (box-index               (if (>= position-box-index 0) position-box-index 0))
         (time                    (elapsed-time-ticks search-start-time-tick-count)) ; no wraparound guard  
         box-square
         neighbor
         successor
         successor-score)
    (if (and search-limit-time-ticks 
             (> time search-limit-time-ticks))
        (terminate-search TIME_LIMIT_EXCEEDED_SCORE))
    (if (>= time display-status-next-time-tick-count)
        (display-status))
    (if (not (search-terminated?))
        (if (< search-count-pushes search-limit-pushes)
            (do..
                  ;(display search-count-pushes "expand" (.data position))
                  ;(display-board board)
                  ;(read-line)
            
                  (if (<= (fill-pointer pusher-reachable-squares-for-search-depth-vector) successor-depth)
                      (vector-push-extend (initialize-timestamp-board nil) pusher-reachable-squares-for-search-depth-vector))

                  (if (and (> position-box-square 0)
                           (tunnel-square? position-box-square position-move-direction))
                      (do.. ; the last moved box is on a tunnel square. disable moves for all other boxes.
                            (for-each-box index
                              (set!   (aref reachable (boxes index)) 0))
                            (set!     (aref reachable position-box-square) (1+ (.timestamp reachable))) ; move this box only
                            ; don't prune successors if the position has its successors generated again at a later stage
                            (set-bit! POSITION_FLAG_REVISIT_BIT (.flags position))))

                  (while (< box-index box-count) ; for each box on the board
                    (set! box-square (boxes box-index))
                    (if (reachable-box? box-square reachable)
                        (for-each-direction direction
                          (set! neighbor (neighbor-square box-square direction))
                          
                          (if (or (and reverse-mode?
                                       (reachable-floor?  neighbor                             reachable)
                                       (reachable-floor?  (neighbor-square neighbor direction) reachable)
                                       (legal-box-square? neighbor)
                                       (not (freezing-move? box-square neighbor))
                                       (do-pull box-index direction))
                                  (and (not reverse-mode?)
                                       (reachable-floor?  (neighbor-square box-square (opposite-direction direction)) reachable)
                                       (legal-box-square-with-no-box? neighbor)
                                       (not (freezing-move? box-square neighbor))                                   
                                       (do-push box-index direction)))                          
                              (do.. 
                                    ;(display "made move" box-index (square-index-to-text (boxes box-index)) (direction-to-char direction) 
                                    ;         successor-push-count simple-lower-bound (+ successor-push-count simple-lower-bound) board-hash-value 
                                    ;         "parent" (.data position)
                                    ;         "open" open-min-value open-max-value)
                                    ;(display-board)
                                    ;(display-status)
                                    ;(read-line)
                              
                                    (set! successor-score (max (if A*-search?
                                                                   (+ successor-push-count simple-lower-bound) ; A* search
                                                                   simple-lower-bound)                         ; greedy search
                                                               position-score)) ; max: so the score doesn't decrease

                                    ; mark pusher reachable squares as not yet calculated for the successor position
                                    (clear-pusher-reachable-squares-for-search-depth successor-depth)

                                    (inc! search-count-pushes) ; update statistics
                               
                                    (if (and (<= successor-score search-limit-score)
                                             (not (search-terminated?))
                                             (set! successor
                                                   (transposition-table-add
                                                      board-hash-value
                                                      ; note that the transposition table item is made with the
                                                      ; actual pusher square value for the box move, and not with
                                                      ; the normalized (top-left) pusher square value. the pusher
                                                      ; square tells where the move takes place on the board, 
                                                      ; something "put-position-on-the-board" must know in order
                                                      ; to repair positions if a new better path to a position is
                                                      ; found, and if this means that it's a different box and/or 
                                                      ; a different move direction which leads to the position.
                                                      pusher-square 
                                                      box-index direction
                                                      successor-push-count successor-score position successor-depth)))
                                        ; the successor position is stored in the transposition table, either because it just has been added,
                                        ; or because it already was there, in which case it might now have been updated with a new best score.
                                        (if (and (zero? simple-lower-bound)
                                                 (or (not reverse-mode?)
                                                     (same-object? successor forward-mode-start-position))) ; true: the pusher is in the solution access area
                                            ; found a solution, possibly via a perimeter node (a leaf position) from a search in the opposite direction
                                            (do.. ; recalculate the simple lower bound. perimeter nodes fake that it's zero.
                                                  (set! simple-lower-bound (calculate-simple-lower-bound)) 
                                                  (if (better-position-score? successor search-solution-position)
                                                      (do.. ; update the position node to ensure it contains the information
                                                            ; for the last box move leading to the solution. if the level
                                                            ; has identical start position and solution position, this means
                                                            ; that the starting position node changes status from being the
                                                            ; first node on the solution path to the last node on the path.
                                                            ;(display "found solution" (.data successor))
                                                            ;(display-board)
                                                            (set! (.push-count        successor) successor-push-count)
                                                            (if (zero? simple-lower-bound) ; otherwise, it's a perimeter node with solution path length in "score"
                                                                (set! (.score         successor) successor-score))
                                                            (set! (.pusher-square     successor) pusher-square)
                                                            (set! (.box-index         successor) box-index)
                                                            (set! (.move-direction    successor) direction)
                                                            (if (and (.parent successor)
                                                                     (not (same-object? (.parent successor) position)))
                                                                ; this cannot happen. if it could happen, then the 
                                                                ; successors of the old and the new parent should be
                                                                ; updated here.
                                                                (error TEXT_INTERNAL_ERROR 1 (.data successor)))
                                                            (set! (.parent successor) position)
                                                            
                                                            (update-path-flags search-solution-position (- POSITION_FLAG_SOLUTION_PATH)) ; remove old best path flags
                                                            (update-path-flags successor                   POSITION_FLAG_SOLUTION_PATH ) ; add    new best path flags
                                                            (set! search-solution-position    successor)
                                                            (set! search-limit-score          successor-score)
                                                            (minimize! open-max-value         search-limit-score)
                                                            (if search-limit-stop-when-solved?
                                                                (terminate-search SOLVED_SCORE)))
                                                      ; use the better or identical score from the existing position
                                                      (set! successor-score (.score successor))))

                                            ; the position isn't a solution
                                            (if (equal? transposition-table-add-result-type 'duplicate)
                                                ; use the better or identical score from the existing position
                                                (set! successor-score (.score successor))
                                                ; the position has been created or updated with a new best score
                                                (if (> successor-score position-score)
                                                    ; put the successor position on the open queue for later expansion
                                                    (open-queue-add successor)
                                                    ; the successor is just as promising as its parent position, and
                                                    ; therefore, it belongs at the front of the open queue. instead
                                                    ; of actually putting it on the queue, it's more efficient to
                                                    ; expand it immediately, like in a depth-first search.
                                                    (if (< successor-push-count search-limit-depth)
                                                        (do.. (if (not (pusher-reachable-squares-calculated-for-search-depth? successor-depth))
                                                                  (calculate-pusher-reachable-squares-for-search-depth        successor-depth))
                                                              (inc! (.flags successor) POSITION_FLAG_PATH) 
                                                              (set! successor-score (perform-search-expand-position successor successor-depth))
                                                              (dec! (.flags successor) POSITION_FLAG_PATH))
                                                        (do.. ; the successor is not expanded. update the best pruned score.
                                                              (minimize! search-best-pruned-score         successor-score)
                                                              (minimize! (.best-forgotten-score position) successor-score))))))
                                        ; the successor position has not been added to the transposition table. update the best pruned score.
                                        (do.. (minimize! search-best-pruned-score         successor-score)
                                              (minimize! (.best-forgotten-score position) successor-score)))

                                    (minimize! result successor-score)

                                    (if reverse-mode?
                                        (undo-pull box-index direction)
                                        (undo-push box-index direction))

                              ))))

                    ; try the next box. the box from the current position was moved first as an attempt
                    ; to produce solutions which look slightly more natural for a human spectator.
                    (set! box-index (if (equal? box-index position-box-index)
                                        0
                                        (1+ box-index)))
                    (if (equal? box-index position-box-index) ; seeing the position box for the second time. skip it.
                        (inc!   box-index)))

                  (if (and (< (.best-forgotten-score        position) DEAD_END_SCORE)
                           (zero? (.successor-count         position)))
                      ; the position has legal successor moves, but none of them
                      ; are stored in the transposition table. put the position back
                      ; on the open queue, unless this would put it at the front of the
                      ; queue, in effect creating an infinite loop. (the position would
                      ; immediately be selected for expansion.)
                      (do..
                        (if (same-object? position          current-position)
                            (put-position-on-the-board      (.parent position)))
                        (set! (.score position)             (max (.score position) (.best-forgotten-score position)))
                        (set! result                        (.score position))
                        ;(display "no successors in table" (.data position)) (display-board)
                        ;(display-transposition-table)
                        (if (<= result                      open-min-value) 
                            ; the position would end up at the front of the open queue again. leave it.
                            (if nil ;(not (same-object? position forward-mode-start-position))
                                (transposition-table-remove position))
                            ; add the position to the open queue at its new position
                            (if (<= result                  search-limit-score)
                                (open-queue-add             position))))
                      (set! (.score position) result))) ; update the score for the position
            ; the number of pushes performed by the search exceeds the limit. terminate the search.
            (terminate-search PUSHES_LIMIT_EXCEEDED_SCORE)))
    result))

(fn perform-search (search-backwards? recycle-positions? emit-zero-pushes-solutions? path-reconstruction-search?)        
  (with-saved-values (pusher-square board box-squares tube-filling-moves reverse-mode? ; restore these values on exit
                      search-limit-depth search-limit-score search-limit-stop-when-solved?)
    (with-timestamp-board (visited (initialize-timestamp-board nil))
      ; save the values of certain global variables before they are overwritten by temporary values
      (let ((original-pusher-square                  pusher-square) 
            (original-box-squares                    box-squares)
            (original-simple-lower-bound             simple-lower-bound)
            position)
        (set! board         (make-array (length board      ) :element-type 'fixnum :initial-contents board))
        (set! box-squares   (make-array (length box-squares) :element-type 'fixnum :initial-contents box-squares))
        (set! reverse-mode? search-backwards?)
        (display (if reverse-mode? TEXT_BACKWARD_SEARCH TEXT_FORWARD_SEARCH))
        (mark-tunnel-squares-on-board reverse-mode?)
        (set! search-node-recycling-enabled? recycle-positions?)  
        (dotimes (index (fill-pointer pusher-reachable-squares-for-search-depth-vector)) ; clear reachable-calculated? flags for all search depths
          (set! (.board-pusher-square (pusher-reachable-squares-for-search-depth index)) 0))
        (if (not search-perimeter-list) ; true: no leaf positions from a preceding search in the opposite direction
            (transposition-table-initialize))
        (open-queue-initialize)   
        (set! current-position         nil) ; no moves performed on the board yet
        (set! search-solution-position nil) ; no solution found yet       
        (set! search-solution-moves    nil) ; no solution found yet       
      
        ; add the forward mode starting position to the transposition table. 
        ; this is the target position for the backward search.
        (set! forward-mode-start-position
              (transposition-table-add 
                 board-hash-value 
                 (normalized-pusher-square (calculate-pusher-reachable-squares-for-search-depth 0))
                 -1 0                                                  ; box-index move-direction: unused here 
                 (if reverse-mode? UNDEFINED_SCORE 0)                  ; push-count
                 (if reverse-mode? UNDEFINED_SCORE simple-lower-bound) ; score
                 nil 0))                                               ; parent and search-depth
      
        ; if it's a path reconstruction search, then the starting position is the target for a backward search.
        ; typically, its simple lower bound score is non-zero, so to signal that it's a target position anyway,
        ; the position is marked as a perimeter node (a leaf position from a search in the opposite direction).
        (if path-reconstruction-search? 
            (do.. (set! (.flags                forward-mode-start-position) POSITION_FLAG_PERIMETER)   
                  (set! (.best-forgotten-score forward-mode-start-position) 0))) ; 0: the "opposite direction search push count" is unused here   
        
        (if (< simple-lower-bound INFINITY_SCORE) ; otherwise, the level is unsolvable      
            (if reverse-mode?
                ; put the reverse mode starting positions on the open queue
                (do.. ; put the reverse mode starting position on the board 
                      (move-pusher               0)
                      (for-each-box index        (move-box index 0))
                      (for-each-box index        (move-box index (goals index)))
                      (set! simple-lower-bound   (calculate-simple-lower-bound))
                      (set! board-hash-value     (calculate-board-hash-value board box-count box-squares))                              
      
                      ; put the reverse mode starting positions on the open queue. 
                      ; the reverse mode starting positions have their boxes on goal squares, 
                      ; and there is one reverse mode starting position for each pusher access area.
                      (for-each-floor-square square
                        (if (and (zero? (aref visited        square))
                                        (not (box-square?    square)))
                            (do.. (move-pusher        square)
                                  ;(display "access area" (square-index-to-text square)) (display-board board) 
                                  (calculate-pusher-reachable-squares-for-search-depth 0)  
                                  (for-each-square square2 ; mark all squares in this access area as visited
                                    (if (reachable-floor? square2 (pusher-reachable-squares-for-search-depth 0))          
                                        (set! (aref visited square2) 1)))
                                    (if (set! position
                                              (transposition-table-add          
                                                board-hash-value                 
                                                (normalized-pusher-square (pusher-reachable-squares-for-search-depth 0))                    
                                                -1 0                 ; box-index move-direction: unused here 
                                                0 simple-lower-bound ; push-count score
                                                nil 0))              ; parent and search-depth
                                        (do.. (open-queue-add position)
                                              ; mark all starting positions as "on current path" to protect them against recycling
                                              (inc! (.flags position) POSITION_FLAG_PATH)) 
                                        (error TEXT_OUT_OF_MEMORY))))))
                ; put the forward mode starting position on the open queue
                (do.. (open-queue-add forward-mode-start-position)
                      (inc! (.flags   forward-mode-start-position) POSITION_FLAG_PATH))))
      
        ; the central search loop.
        ; repeatedly select the most promising position for expansion and generate its successors.
        (while (set! position (open-queue-select-position-for-expansion nil))
          (open-queue-remove         position)
          (put-position-on-the-board position)
          ;(display "dequeued position" (.data position))
          ;(display-board board)
          ;(display-status)
          (calculate-pusher-reachable-squares-for-search-depth 0) ; a precondition for "perform-search-expand-position"
          (perform-search-expand-position position 0))
      
        ; if a solution has been found, then check that its path still is intact
        (put-solution-position-on-the-board)
      
        ; make the solution path moves if a solution has been found  
        (if (or search-solution-position                 ; true: found a solution
                (and (zero? original-simple-lower-bound) ; true: start position and end position are identical
                     emit-zero-pushes-solutions?))       ; true: don't perform another search in the opposite direction for a non-trivial solution
            (if reverse-mode?
                ; backward search solution
                (let ((box-index-map                  (make-array box-count :element-type 'fixnum :initial-element -1))
                      (position                       search-solution-position)
                      (push-count                     0))
                  ; map reverse mode box numbers to forward mode box numbers          
                  (put-position-on-the-board          search-solution-position) 
                  (for-each-box index
                    (for-each-box index2
                      (if (equal? (boxes index2)      (aref original-box-squares index))
                          (set!   (aref box-index-map index2) index))))
            
                  ; create the list of moves, including non-pushing moves between the pushes           
                  (set! search-solution-moves (reverse! (copy-list tube-filling-moves)))
                  (move-pusher original-pusher-square)
                  (while (and position (>= (.box-index position) 0))
                    (dolist (move (calculate-pusher-path pusher-square (.pusher-square position)))
                      (push move search-solution-moves))
                    (set! (.push-count     position) (inc! push-count))
                    (push (make-move :direction (opposite-direction (.move-direction position))
                                     :box-index (aref box-index-map (.box-index      position))) 
                          search-solution-moves)
                    (do-push (.box-index position)   (opposite-direction (.move-direction position)))
                    (set! position                   (.parent position))
                    ; if the start position and the end position are identical, 
                    ; and the start position has been reached, then exit the loop
                    (if   (same-object? position     search-solution-position)
                          (set! position             nil)))
                  (set! search-solution-moves        (if search-solution-moves
                                                         (reverse! search-solution-moves)
                                                         t))) ; t: trivial zero-pushes solution
                ; forward search solution
                (do.. ; reset the position on the board to the starting position   
                      (put-position-on-the-board forward-mode-start-position)
                      (move-pusher original-pusher-square)             
      
                      (let ((position (if search-solution-position
                                          (reverse-path! search-solution-position))))
                        (if (same-object? position forward-mode-start-position) 
                            (set! position (.parent position))) ; skip starting position
                        ; create the list of moves, including non-pushing moves between the pushes           
                        (set! search-solution-moves (reverse! (copy-list tube-filling-moves)))
                        (while position
                          (dolist (move (calculate-pusher-path 
                                           pusher-square 
                                           (neighbor-square (.pusher-square position) 
                                                            (opposite-direction (.move-direction position)))))
                            (push move search-solution-moves))
                          (push (make-move :direction      (.move-direction position)
                                           :box-index      (.box-index      position)) 
                                search-solution-moves)              
                          (do-push (.box-index position)   (.move-direction position))              
                          (if (same-object? position       search-solution-position) 
                              ; the start position and the end position are identical
                               (set! position               nil) ; exit loop              
                              ; advance to next position on the path
                             (set! position                (.parent position))))
                        (if (zero? simple-lower-bound) 
                            ; the found solution goes all the way to the goal position
                            (set! search-solution-moves (if search-solution-moves 
                                                            (reverse! search-solution-moves)
                                                            t)) ; t: trivial zero-pushes solution                          
                            ; the found solution is via a perimeter node (a leaf position) from a
                            ; preceding backward search. construct the path from the perimeter node
                            ; to the goal position by running a new backward search, this time
                            ; targeting the perimeter node, which matches the current game position.
                            (let ((moves-from-start-to-perimeter-node search-solution-moves))
                              ; set up search limits for the path reconstruction search
                              (set! search-limit-score             (.score search-solution-position))                            
                              (set! search-limit-depth             (- search-limit-score (.push-count search-solution-position)))
                              (set! search-limit-stop-when-solved? t)                            
                              ;(display "score limit" search-limit-score "depth limit" search-limit-depth (.data search-solution-position))
                              ; make special preparations for the recursive call of "perform-search"
                              (set! tube-filling-moves       nil) ; tube-filling moves have already been put on the list of moves                      
                              (set! search-perimeter-list    nil) ; drop all remaining perimeter nodes
                              ; create the moves from the perimeter position to the goal position
                              (if (set! search-solution-moves (perform-search t nil nil t))                            
                                  (set! search-solution-moves 
                                        (append! (reverse! moves-from-start-to-perimeter-node)
                                                           (if (list? search-solution-moves)
                                                               search-solution-moves)))
                                   (display TEXT_SOLUTION_PATH_CONSTRUCTION_FAILED)))))))))))
  
                      
  (set! simple-lower-bound (calculate-simple-lower-bound))                           ; restore current value
  (set! board-hash-value   (calculate-board-hash-value board box-count box-squares)) ; restore current value
  search-solution-moves) ; return solution, if any

(fn initialize-search ()
  (while (<= (fill-pointer pusher-reachable-squares-for-search-depth-vector) 0)
    (vector-push-extend (initialize-timestamp-board nil) pusher-reachable-squares-for-search-depth-vector))  
; (set! search-start-time-tick-count       (get-tick-count)) ; solver time statistics must include level preprocessing, see "load-level"
  (set! display-status-next-time-tick-count display-status-time-interval-tick-count)  
  (set! search-best-pruned-score           INFINITY_SCORE)
  (set! search-count-duplicate-positions   0)
  (set! search-count-new-best-paths        0)
  (set! search-count-positions             0)   
  (set! search-count-pushes                0)
  (set! search-count-recycled-positions    0)  
  (set! search-count-path-repairs          0)    
  (set! search-limit-depth                 INFINITY_SCORE) 
  (set! search-limit-pushes                most-positive-fixnum)
  (set! search-limit-score                 1000) ;...INFINITY_SCORE)
  (set! search-no-progress-rover           0)
  (set! search-perimeter-list              nil) 
  (set! search-solution-moves              nil)  
  (set! search-solution-position           nil)  
  (set! search-debug-path                  nil)    
  (if (zero? simple-lower-bound)
      (do.. (displayln (if (zero? (move-list-push-count tube-filling-moves))
                           TEXT_TRIVIAL_SOLUTION_LINE_1
                           TEXT_TRIVIAL_SOLUTION_LINE_1_AFTER_PREPROCESSING))
            (displayln TEXT_TRIVIAL_SOLUTION_LINE_2)))
  (if (>= simple-lower-bound INFINITY_SCORE)
      (displayln TEXT_UNSOLVABLE_LEVEL))
  (< simple-lower-bound INFINITY_SCORE) ; true: the level has not been proved unsolvable
)

(fn finalize-search ()
  (let ((transposition-table-item-count transposition-table-item-count)) ; save count for reporting before the table is cleared
    (transposition-table-finalize)
    (display-status nil transposition-table-item-count)
    (if search-solution-moves
        (displayln (format nil TEXT_FORMAT_SOLUTION_MOVES_PUSHES 
                           (move-list-move-count search-solution-moves) 
                           (move-list-push-count search-solution-moves)))
        (displayln (search-state-to-text NOT_SOLVED_SCORE)))))

(fn solve-level (level)
  (local-fns (
    (solve ()
      (if (initialize-search)
          (do.. (if (equal? search-strategy 'backward)
                    (perform-search t t t nil)
                    (if (equal? search-strategy 'perimeter)
                        ; perimeter search. first perform a backward search.
                        (with-saved-values (search-limit-time-ticks transposition-table-size-limit)
                          ; only spend a certain percentage of the time on the backward search
                          (if search-limit-time-ticks
                              (set! search-limit-time-ticks 
                                    (truncate (* PERIMETER_SEARCH_BACKWARD_SEARCH_TIME_PCT search-limit-time-ticks) 100)))
                          ; limit the number of positions generated during the backward search to a reasonable number
                          (minimize! transposition-table-size-limit DEFAULT_TRANSPOSITION_TABLE_SIZE_LIMIT)     
                          (if (not (perform-search t nil nil nil)) ; true: backward search didn't find a soluton
                              (save-perimeter-positions)))))       ; save leaf positions and use them as targets for a forward search
                (if (and (not        search-solution-moves)        ; true: not solved yet
                         (not-equal? search-strategy 'backward))   ; true: perimeter search or forward search
                    (perform-search nil t t nil))
                (finalize-search)
                search-solution-moves)))) ; return the found solution, if any              
    
    (let ((result nil))
      (if (load-level level nil)
          (do..  (if (not-equal? (level-index level) (1- level-start-index))
                     (nl))
                 (if (level-name level) 
                     (displayln (level-name level)))
                 (display-board board)
                 ;(display "pusher's reachable squares" 
                 ;         (sort! (mapcar #'square-index-to-text (pusher-reachable-squares-list)) #'string<))
                 (if (set! result (solve)) 
                     (let ((move-count (move-list-move-count result))
                           (push-count (move-list-push-count result)))
                       (set! (level-solution level) 
                             (make-solution 
                               :name       (format nil TEXT_FORMAT_SOLUTION_MOVES_PUSHES move-count push-count)
                               :move-count move-count
                               :push-count push-count
                               :moves      (moves-to-text (if (list? result) result))
                               :notes      (let ((notes nil))
                                             (push (format nil TEXT_FORMAT_SOLVER              TEXT_PROGRAM_NAME_AND_VERSION        ) notes)
                                             (push (format nil TEXT_FORMAT_SOLVER_TIME_SECONDS (round search-time-milliseconds 1000)) notes)
                                             (if (or (zero? push-count)             ; true: trivial zero-pushes solution
                                                     (not search-solution-position) ; true: trivial zero-pushes solution after preprocessing
                                                     (and (zero? search-count-recycled-positions)
                                                          (not (position-on-perimeter? search-solution-position))))
                                                 (push (format nil TEXT_FORMAT_OPTIMALITY      TEXT_PUSH_OPTIMAL                    ) notes))
                                             (reverse! notes))
                               :time       search-time-milliseconds))))))
      result)))  

(fn solve-level-file (input-file-name-parameter output-file-name-parameter start end &optional (input-file-levels-parameter nil)) ; start, end: 1-based, both inclusive
  (let ((count-solved  0)
        (count-failed  0)
        (moves         0)
        (pushes        0)
        (time          0))
    (set! input-file-name   nil)                            ; reset cached file name and levels
    (set! input-file-levels nil)
    (set! input-file-levels (or input-file-levels-parameter ; true: levels already loaded. accumulate solutions for this level file.
                                (and input-file-name-parameter (load-levels-from-file input-file-name-parameter))))
    (set! input-file-name   input-file-name-parameter)      ; save the file name for the loaded levels 
    (set! output-file-name  output-file-name-parameter)     ; save output file name for successive invocations of "(solve ...)"
    (nl)    
    (dolist (level input-file-levels)
      (if (and (>=      (level-index level) (1- start))
               (or (not end)
                   (<=  (level-index level) (1- end))))
          (if (solve-level level)
              (do.. (inc! count-solved) 
                    (inc! moves  (solution-move-count (level-solution level)))
                    (inc! pushes (solution-push-count (level-solution level)))            
                    (inc! time   (solution-time       (level-solution level)))
                    (if   (solution-moves (level-solution level))
                          (dolist (moves-line (moves-to-text-lines (solution-moves (level-solution level))))
                            (displayln moves-line))))                        
              (do.. (inc! count-failed)
                    (inc!  time  search-time-milliseconds))))) ; include time spent on unsolved levels
    (if output-file-name
        (save-levels-to-file input-file-levels output-file-name))
    (nl)
    (displayln (format nil TEXT_FORMAT_SOLVER_STATISTICS count-solved (+ count-solved count-failed) moves pushes (round time 1000)))
    count-solved)) ; return the number of solved levels

(fn initialize-solver (input-file-parameter output-file-parameter
                       strategy-parameter transposition-table-size-limit-parameter time-limit-seconds-parameter
                       start-index-parameter end-index-parameter) ; start, end: 1-based, both inclusive
  (local-fns ((char-to-value (char value) (set! (aref BOARD_CHAR_VALUE_MAP (char-int char)) value)
                                          (value-to-char value char)) 
              (value-to-char (value char) (set! (aref BOARD_VALUE_CHAR_MAP (bit-shift-arithmetic value (- DIRECTION_COUNT))) char)))
    (let ()
      (set! BOARD_CHAR_VALUE_MAP  (make-array  (1+ MAX_ASCII) :element-type 'fixnum :initial-element EMPTY_SQUARE))  
      (set! BOARD_VALUE_CHAR_MAP  (make-array  (bit-shift-arithmetic (1+ BOARD_OBJECT_BITS) (- DIRECTION_COUNT)) 
                                               :element-type 'character :initial-element BOARD_CHAR_UNKNOWN))    
      (char-to-value BOARD_CHAR_BOX            BOX+FLOOR_VALUE)
      (char-to-value BOARD_CHAR_BOX_ON_GOAL    BOX+GOAL+FLOOR_VALUE)  
      (char-to-value BOARD_CHAR_FLOOR1         FLOOR_VALUE)      
      (char-to-value BOARD_CHAR_FLOOR2         FLOOR_VALUE)
      (char-to-value BOARD_CHAR_FLOOR3         FLOOR_VALUE)        
      (char-to-value BOARD_CHAR_GOAL           GOAL+FLOOR_VALUE)      
      (char-to-value BOARD_CHAR_PUSHER         PUSHER+FLOOR_VALUE)      
      (char-to-value BOARD_CHAR_PUSHER_ON_GOAL PUSHER+GOAL+FLOOR_VALUE)
      (char-to-value BOARD_CHAR_WALL           WALL)
      (value-to-char FLOOR_VALUE               BOARD_CHAR_FLOOR2)
      (value-to-char EMPTY_SQUARE              BOARD_CHAR_FLOOR3)
      ; non-wall squares without "FLOOR_VALUE" are not reachable by the pusher. they are only decorative.
      (value-to-char BOX                       BOARD_CHAR_BOX)            
      (value-to-char (+ BOX GOAL)              BOARD_CHAR_BOX_ON_GOAL)
      (value-to-char GOAL                      BOARD_CHAR_GOAL)    
      (value-to-char (+ PUSHER GOAL)           BOARD_CHAR_PUSHER_ON_GOAL) 
    
      (set! OPPOSITE_DIRECTION_MAP             (make-array DIRECTION_COUNT :element-type 'fixnum))
      (for-each-direction direction
        (set! (aref OPPOSITE_DIRECTION_MAP direction) 
              (mod (+ direction (bit-shift-arithmetic DIRECTION_COUNT -1)) DIRECTION_COUNT)))      
  
      (set! DIRECTION_TO_UPPERCASE_CHAR        (make-array (length DIRECTION_TO_LOWERCASE_CHAR)))
      (for-each-direction direction 
        (set! (aref DIRECTION_TO_UPPERCASE_CHAR direction) (char-upcase (aref DIRECTION_TO_LOWERCASE_CHAR direction))))
  
      (set! neighbor-square-offsets            (make-array DIRECTION_COUNT :element-type 'fixnum))
      (set! zobrist-bit-strings                (make-array 0 :element-type 'integer :adjustable t :fill-pointer t :initial-element 0))
    
      (if (and (string? input-file-parameter)  (not-equal? "" input-file-parameter))
          ; if the input file name hasn't changed, then keep the currently loaded levels in order to
          ; accumulate solutions for this level collection. (implementation limitation: it's not checked
          ; whether the input file has been modified after it was loaded.)
          (if (not-equal? input-file-parameter input-file-name)
              (do.. (set! input-file-name      input-file-parameter)
                    (set! input-file-levels    nil)))
          (do.. (set! input-file-name          nil)
                (set! input-file-levels        nil)))
      (if (and (string? output-file-parameter) (not-equal? "" output-file-parameter))
          (set! output-file-name               output-file-parameter)
          (set! output-file-parameter          nil))    
      (if (member strategy-parameter SEARCH_STRATEGIES)
          (set! search-strategy                strategy-parameter)
          (set! search-strategy                DEFAULT_SEARCH_STRATEGY))
      (if (number? transposition-table-size-limit-parameter)
          (set! transposition-table-size-limit transposition-table-size-limit-parameter)
          (set! transposition-table-size-limit DEFAULT_TRANSPOSITION_TABLE_SIZE_LIMIT))
      (if (number? time-limit-seconds-parameter)
          (set! search-limit-time-seconds      time-limit-seconds-parameter)
          (set! search-limit-time-seconds      nil)) 
      (if (number? search-limit-time-seconds)
          (set! search-limit-time-ticks        (milliseconds-to-tick-count (* 1000 search-limit-time-seconds)))
          (set! search-limit-time-ticks        nil))
      (if (number? start-index-parameter)    
          (set! level-start-index              start-index-parameter)
          (set! level-start-index              1))
      (if (number? end-index-parameter)    
          (set! level-end-index                end-index-parameter)
          (set! level-end-index                nil)) ; nil: process all levels from start index to end of file
    
      (set! transposition-table          (make-hash-table :test 'equal :rehash-size 1.25)) ; :size transposition-table-size-limit))
      (set! open-buckets-vector          (make-array 0 :adjustable t :fill-pointer t :initial-element nil))  
      (set! pusher-reachable-squares-for-search-depth-vector
           (make-array 0 :adjustable t :fill-pointer t :initial-element nil)) 
      (set! freezing-move-timestamp-boards-vector (make-array AXIS_COUNT :initial-element nil)) ; one timestamp board for each axis
    
      (set! random32-seed 0)           ; fixed start value for reproducible results
      (dotimes (index 256) (random32)) ; warm up the random number generator
      (set! zobrist-bit-strings-random-seed random32-seed)) ; save the random number seed used for producing the Zobrist bit strings
  
      (set! display-status-time-interval-tick-count
            (milliseconds-to-tick-count (* 1000 DISPLAY_STATUS_TIME_INTERVAL_SECONDS)))))

(fn finalize-solver ()
  (set! distance-queue-item-pool      nil)
  (set! timestamp-board-pool          nil)
  (set! transposition-table-item-pool nil)
)

(fn solve (&key (input    input-file-name)                ; input  file name, levels in "SOK" file format
                (output   output-file-name)               ; output file name, levels and solutions in "SOK" file format
                (strategy search-strategy)                ; backward, forward, or perimeter (default) 
                (table    transposition-table-size-limit) ; transposition table size limit, number of positions
                (time     search-limit-time-seconds)      ; time limit, seconds
                (start    level-start-index)              ; first level index to solve, 1-based, inclusive
                (end      level-end-index))               ; last  level index to solve, 1-based, inclusive
  (initialize-solver input output strategy table time start end)
  (solve-level-file  input-file-name output-file-name level-start-index level-end-index input-file-levels)
  (finalize-solver))

; example:
   (in-package "Sokoban")  ; change package, i.e., namespace, to "Sokoban"
;  (solve :input "C:\\Temp\\Microban.sok" :output "C:\\Temp\\Microban-solutions.sok")
;  (solve :table 5000 :time 10 :input "C:\\Temp\\Microban.sok" :output "C:\\Temp\\Microban-solutions.sok")

(fn test-zobrist-hash-values (&key (box-count 3) (board-size 256) (zobrist-bit-string-size 64))
  (let ((table (make-hash-table :test #'equal))
        (count 0))
    (local-fns (
                
      ;(write-hash-value (hash-value squares)          
      ;)                         
                        
      (table-add! (hash-value)          
        (let ((item (hash-table-lookup hash-value table)))
          (if item ; true: duplicate zobrist hash values
              (do.. (display "DUPLICATES" item) ; display duplicates
                    (read-line))
              (set! item hash-value)) 
          (hash-table-add! hash-value item table)))
              
      (try-box (box-index square hash-value squares box-count)             
         (if (>= box-index box-count)
             (do.. (inc! count)
                   ;(if (zero? (bitand count 1023))
                   ;    (display count ":" squares hash-value))                                 
                   (table-add! hash-value)
                   
             )      
             (while (< square board-size) 
                (if (zero? box-index)
                    (display "Boxes:" box-count "Box:" box-index "Square:" square "Positions:" count))
                (try-box (1+ box-index) (1+ square) ; next box, starting from the next square
                         (bitxor hash-value (aref zobrist-bit-strings square))
                         (+ (* squares (max 10 board-size)) square)
                         box-count)
                (inc! square)))))
             
    (let () 
      (initialize-solver nil nil nil 0 0 0 0)
      (while (<= (fill-pointer zobrist-bit-strings) board-size)
        (vector-push-extend (make-zobrist-bit-string zobrist-bit-string-size) zobrist-bit-strings))
      (nl)
      (dotimes (index box-count) ; for each number of boxes 1..box-count
        (try-box 0 0 0 0 (1+ index)))
      (display "Count" count)
    ))))

; box-count: 5  board-size: 256  positions:  8,987,138,112 
;(test-zobrist-hash-values :board-size 8)

(fn save-zobrist-hash-values (&key (board-size 256) (zobrist-bit-string-size 64) (file-name "C:\\Temp\\Zobrist.txt"))
  (let ((lines nil))    
    (initialize-solver nil nil nil 0 0 0 0)
    (while (<= (fill-pointer zobrist-bit-strings) board-size)
      (vector-push-extend (make-zobrist-bit-string zobrist-bit-string-size) zobrist-bit-strings))
    
    (dotimes (index (length zobrist-bit-strings))
      (push (format nil "~a ~a ~V,'0X" index (aref zobrist-bit-strings index) (/ zobrist-bit-string-size 4) (aref zobrist-bit-strings index)) lines))
    ;(display lines)
    (save-text-lines-to-file (reverse! lines) file-name)))
;(save-zobrist-hash-values :zobrist-bit-string-size 64)
