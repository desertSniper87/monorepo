#|
  YAMSGen   - Yet Another Mini Sokoban Generator
  Author    : Brian Damgaard, Denmark
  E-mail    : BrianDamgaard@jubii.dk
  Copyright : (c) 2017 by Brian Damgaard, Denmark
  Version   : 1.0
  License   : GPL
  ______________________________________________________________________________

  YAMSGen - Yet Another Mini Sokoban Generator
  Copyright (c) 2017 Brian Damgaard, Denmark

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

  Usage

    In a Common Lisp implementation with both an interpreter and a compiler,
    the puzzle generator must be compiled before it can be used, otherwise the
    generator is too slow. For instance, in CLisp from "http://clisp.org/"
    (retrieved 2014-08-13) the procedure looks like this:
      (compile-file "yamsgen.lsp") ; add the appropriate path name prefix
      (load         "yamsgen.fas") ; add the appropriate path name prefix

    To run the generator, call the function "main" in the "Sokoban" package with
    the following optional parameters:

      (main        ; optional parameters:
         width     ; board width
         height    ; board height
         seed      ; random number seed
      )

    Example:
      (in-package "Sokoban")  ; change package, i.e., namespace, to "Sokoban"
      (main 12 9)

    This call attempts to generate puzzles with the board size 12 x 9. Since 
    no seed value is supplied, the random number generator uses the current time 
    as seed.
  ______________________________________________________________________________
|#

(defpackage "Sokoban")
(in-package "Sokoban")

; general utilities
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
(mc            tick-count-to-milliseconds
                         (tick-count) `(* 1000 (/ ,tick-count internal-time-units-per-second))) 

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
(mc            clear-bit! 
                         (index value)
                         `(setf ,value (bitandc1 (bit-shift-arithmetic 1 ,index) ,value)))
(mc            const     (&rest r) `(defconstant ,@r))
(mc            dec!      (&rest r) `(decf ,@r)) 
(mc            display1  (item) `(princ ,item))
(mc            displayln (item) `(progn (display1 ,item) (nl)))
(mc            display  (&rest r)
                         `(do.. (dolist (item (list ,@r)) (display1 item) (display1 " ")) (nl)))
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
(mc            while..   (expr &rest body)
                         (let ((loop (new-symbol)))
                           `(tagbody ,loop
                              (if ,expr (do.. ,@body (go ,loop))))))
(mc            zero?     (value) `(zerop ,value))

)

; minimal standard random number generator
(const MSRNG_A 16807)      ; A = 7^5 = 16807.                           don't change the value unless you know what you're doing.
(const MSRNG_M 2147483647) ; M = 2^31 - 1 = 2147483647, a prime number. don't change the value unless you know what you're doing.

(var minimal-standard-random-number-generator-seed 1) ; the seed must be [1..M-1]

(fn minimal-standard-random-number-generator (&optional limit)
  (set! minimal-standard-random-number-generator-seed
        (mod (* MSRNG_A minimal-standard-random-number-generator-seed) MSRNG_M)) ; Common Lisp has big integers, so the multiplication cannot cause an integer overflow
  (if limit ; true: return a value [0..limit-1]
      (remainder (1- minimal-standard-random-number-generator-seed) limit)
      (1- minimal-standard-random-number-generator-seed))) ; return [0..M-2]. the seed is [1..M-1].

(fn set-minimal-standard-random-number-generator-seed! (seed)
  (set! minimal-standard-random-number-generator-seed (max 1 (mod seed (1- MSRNG_M))))) ; the seed must be [1..M-1]

; shuffle vectors and lists
(fn shuffle-vector! (vector) ; algorithm: Knuth shuffle
  (let ((vector-length (length vector))
        index2)
    (dotimes (index (- vector-length 2))
      (set! index2 (+ index (minimal-standard-random-number-generator (- vector-length index))))
      (rotatef (aref vector index) (aref vector index2)))
    vector))

(fn shuffle-list (list)
  (if list
      (coerce (shuffle-vector! (make-array (length list) :initial-contents list)) 'list)))

; 2D rotations and reflections
(const 2D_TRANSFORMATIONS ; clock-wise rotation
       '(2DTransformation0                          2DTransformation90
         2DTransformation180                        2DTransformation270
         2DTransformation0FlippedHorizontally       2DTransformation90FlippedHorizontally
         2DTransformation180FlippedHorizontally     2DTransformation270FlippedHorizontally))

(const 2D_TRANSFORMATION_COUNT ; enum returns the number of enumeration constants
       (enum 2DTransformation0                      2DTransformation90
             2DTransformation180                    2DTransformation270
             2DTransformation0FlippedHorizontally   2DTransformation90FlippedHorizontally
             2DTransformation180FlippedHorizontally 2DTransformation270FlippedHorizontally))

(fn calculate-2D-transformation (transformation col row width height &optional flip-horizontally?) ; clock-wise rotation
  (if (= transformation 2DTransformation0) ; 0 degrees, possibly flipped horizontally
      (if flip-horizontally?
          (cons (- width col 1) row)
          (cons col             row))
      (if (<= transformation 2DTransformation270) ; 90, 180, 270 degrees, possibly flipped horizontally
          (calculate-2D-transformation (1- transformation) (- height row 1) col height width flip-horizontally?) ; rotate clock-wise
          (if (< transformation 2D_TRANSFORMATION_COUNT) ; 0, 90, 180, 270 degrees, flip horizontally
              (calculate-2D-transformation (mod transformation 2DTransformation0FlippedHorizontally) col row width height t)
              (calculate-2D-transformation (mod transformation 2D_TRANSFORMATION_COUNT             ) col row width height flip-horizontally?)))))

(fn display-2D-transformations (col row width height)
  (dotimes (i (1+ 2D_TRANSFORMATION_COUNT))
    (format t "~% ~40A ~D: ~A" (nth (mod i 2D_TRANSFORMATION_COUNT) 2D_TRANSFORMATIONS) i (calculate-2D-transformation i col row width height))))

;(display-2D-transformations 0 0 3 4) ; example: transformations of the coordinates [0,0] for a 3 columns x 4 rows grid

(fn spiral-order (width height) ; returns the spiral order of the elements in a [width x height] matrix, starting from the top-left corner and then going right, down, left, and up
  (let ((result nil)
        (direction 0)
        (left 0) (top 0) (right (1- width)) (bottom (1- height)))
    (while.. (and (<= left right) (<= top bottom))
      (case direction
        (0 (dotimes (offset (- right  left  -1)) (push (cons (+ left  offset) top)               result)) ; going right, top row
           (inc! top))
        (1 (dotimes (offset (- bottom top   -1)) (push (cons right            (+ top offset))    result)) ; going down, right column
           (dec! right))
        (2 (dotimes (offset (- right  left  -1)) (push (cons (- right offset) bottom)            result)) ; going left, bottom row
           (dec! bottom))
        (3 (dotimes (offset (- bottom top   -1)) (push (cons left             (- bottom offset)) result)) ; going up, left column
           (inc! left)))
      (set! direction (mod (1+ direction) 4))) ; advance to the next direction. order: right, down, left, up.
    (reverse! result)))

(fn display-spiral-order (width height) ; for testing the spiral order calculation
  (let ((array (make-array (list width height)))
        (order (spiral-order width height))
        (count 0))
    (dolist (col-row order)
      (set! (aref array (first col-row) (rest col-row)) (inc! count)))
    (dotimes (row height)
      (dotimes (col width)
        (format t "~5D" (aref array col row)))
      (nl))))

; Sokoban board constants
(const         MIN_BOARD_WIDTH                 3)
(const         MIN_BOARD_HEIGHT                3)
(const         MAX_BOARD_WIDTH                 50)
(const         MAX_BOARD_HEIGHT                50)

(const         DIRECTION_UP                    0) ; directions: 0 .. DIRECTION_COUNT - 1
(const         DIRECTION_LEFT                  1)
(const         DIRECTION_DOWN                  2)
(const         DIRECTION_RIGHT                 3)
(const         DIRECTION_COUNT                 4) ; precondition: even
(const         AXIS_COUNT                      (bit-shift-arithmetic DIRECTION_COUNT -1))
(const         AXIS_MASK                       (1- (bit-shift-arithmetic 1 (1- AXIS_COUNT))))

(const         TOP_LEFT                        1) ; rectangle corners
(const         TOP_RIGHT                       2)
(const         BOTTOM_LEFT                     4)
(const         BOTTOM_RIGHT                    8)

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
(const         ILLEGAL_BOX_SQUARE+FLOOR_VALUE  (+ ILLEGAL_BOX_SQUARE FLOOR_VALUE))
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
(const         BOARD_CHAR_ILLEGAL_BOX_SQUARE   #\_)
(const         BOARD_CHAR_PUSHER               #\@)
(const         BOARD_CHAR_PUSHER_ON_GOAL       #\+)
(const         BOARD_CHAR_WALL                 #\#)
(const         BOARD_CHAR_UNKNOWN              #\?)
(const         MAX_ASCII                       127)

(const         BOARD_CHAR_VALUE_MAP            `((,BOARD_CHAR_WALL               . ,WALL)
                                                 (,BOARD_CHAR_FLOOR2             . ,FLOOR_VALUE)
                                                 (,BOARD_CHAR_FLOOR1             . ,EMPTY_SQUARE)
                                                 (,BOARD_CHAR_BOX                . ,BOX+FLOOR_VALUE)
                                                 (,BOARD_CHAR_BOX_ON_GOAL        . ,BOX+GOAL+FLOOR_VALUE)
                                                 (,BOARD_CHAR_GOAL               . ,GOAL+FLOOR_VALUE)
                                                 (,BOARD_CHAR_PUSHER             . ,PUSHER+FLOOR_VALUE)
                                                 (,BOARD_CHAR_PUSHER_ON_GOAL     . ,PUSHER+GOAL+FLOOR_VALUE)
                                                 (,BOARD_CHAR_ILLEGAL_BOX_SQUARE . ,ILLEGAL_BOX_SQUARE+FLOOR_VALUE)
                                                ))

(var           COMPASS_DIRECTIONS_COL            #( 0 -1  0  1)) ; up left down right. really a constant, but defined as "var" for convenience to avoid redefinition warnings on recompilation.
(var           COMPASS_DIRECTIONS_ROW            #(-1  0  1  0)) ; up left down right. really a constant, but defined as "var" for convenience to avoid redefinition warnings on recompilation.

; texts
(const         TEXT_BOARD_GENERATOR_INSTRUCTIONS "Sokoban board generator:~%  (in-package \"Sokoban\")~%  (main &optional width height seed)")
(const         TEXT_DONE                       "Done")
(const         TEXT_ERROR_NO_PUZZLE_FILE_NAME  "Save puzzle to file: Filename not specified.")
(const         TEXT_FORMAT_BOARD_GENERATOR_STATISTICS "Stats: 3x3 regions: ~D. Fill region attempts: ~D.~%")
(const         TEXT_FORMAT_PROMPT_COMMAND      "~%~A ~A. Press [Enter] to generate ~D board~P of size [~D+2 x ~D+2].~%Commands: W)idth H)eight sE)ed R)ound S)ave F)ilename Q)uit + [Enter]: ")
(const         TEXT_FORMAT_PROMPT_HEIGHT       "Type board height, discounting the border, and then press [Enter]. (The height is rounded to multiple of ~D.):")
(const         TEXT_FORMAT_PROMPT_PUZZLE_FILE_NAME "Current filename: ~A~%Type filename for saving puzzles to a file, and then press [Enter]. (Puzzles will be appended to the file.):")
(const         TEXT_FORMAT_PROMPT_WIDTH        "Type board width, discounting the border, and then press [Enter]. (The width is rounded to multiple of ~D.):")
(const         TEXT_FORMAT_PUZZLES_SAVED_TO_FILE "~%~D puzzle~P saved to file: ~A~%")
(const         TEXT_FORMAT_WIDTH_HEIGHT_SEED   "~%Width: ~D+2  Height: ~D+2  Seed: ~D~%")
(const         TEXT_NONE                       "None")
(const         TEXT_PROGRAM_TITLE              "ASMSGen")
(const         TEXT_PROGRAM_VERSION            "1.0")
(const         TEXT_PROMPT_CANDIDATES_PER_ROUND "Type number of board candidates to generate in each round, and then press [Enter]:")
(const         TEXT_PROMPT_RANDOM_SEED          "Type random number seed for the board generator, and then press [Enter]:")

; map characters to board square values, and vice versa
(mc object-bits          (value)               `(bitand BOARD_OBJECT_BITS    ,value)) ; Sokoban board square objects: floor wall pusher box goal

(mc char-to-square-value (char)                `(rest (assoc ,char BOARD_CHAR_VALUE_MAP)))
(mc square-value-to-char (square-value)        `(if ,square-value (first (rassoc (object-bits ,square-value) BOARD_CHAR_VALUE_MAP)) BOARD_CHAR_UNKNOWN))

; templates as text
; coordinate system for templates:
; [0,0] is the bottom-left cell, the first character in the last string for each template.

(const TEMPLATES_AS_TEXT ; preconditions: all templates have the same size. template size >= 3x3.
  '( ; Joshua Taylor
     ; The Procedural Generation of Interesting Sokoban Levels
     ("     " " --- " " --- " " --- " "     ")   ; 00 ; disabled by default
     ("     " " #-- " " --- " " --- " "     ")   ; 01 ; disabled by default
     ("   --" " ##--" " --- " " --- " "     ")   ; 02
     ("     " " ### " " --- " " --- " "     ")   ; 03
     ("     " " ### " " #-- " " #-- " "     ")   ; 04
     ("  -  " " #-- " "---- " " --# " "     ")   ; 05
     ("     " " #-- " "---- " " #-- " "     ")   ; 06
     ("  -  " " #-- " "---- " " #-# " "  -  ")   ; 07
     ("  -  " " #-# " "-----" " #-# " "  -  ")   ; 08
     ("  -  " " #-# " " #_--" " ### " "     ")   ; 09 "_": a floor, but considered a wall square by the floor connectivity calculation
     ("     " " ### " "-----" " ### " "     ")   ; 10
     ("     " " ----" " -#--" " --- " "     ")   ; 11 ; disabled by default
     ("     " " ### " " ### " " ### " "     ")   ; 12 ; disabled by default
     ("     " " ### " " #-- " "---- " "--   ")   ; 13
     (" - - " " --- " " #-# " " --- " " - - ")   ; 14
     ("     " " ### " " ### " " --- " " --- ")   ; 15
     ("     " " ### " "--#--" " --- " " --  "))) ; 16
(const TEMPLATE_COUNT  (length TEMPLATES_AS_TEXT))
(const TEMPLATE_WIDTH  (length (first (first TEMPLATES_AS_TEXT))))
(const TEMPLATE_HEIGHT (length (first TEMPLATES_AS_TEXT)))

(const DISABLED_TEMPLATES '(0 1 11 12)) ; all-floors, all-walls, and single-wall templates are disabled. the generated boards seem to be better that way.

(mc for-each-template-square (col row &rest body)
  `(dotimes (,col TEMPLATE_WIDTH)
     (dotimes (,row TEMPLATE_HEIGHT)
       (do.. ,@body))))

(mc template-square-as-text (template col row)
  `(aref (nth ,row (nth ,template TEMPLATES_AS_TEXT)) ,col))

(fn display-template-as-text (template)
  (nl)
  (dotimes (row TEMPLATE_HEIGHT)
    (dotimes (col TEMPLATE_WIDTH)
      (display1 (template-square-as-text template col row)))
      (nl)))

(fn display-templates-as-text ()
  (dotimes (template TEMPLATE_COUNT)
    (format t "~%~D" template)
    (display-template-as-text template)))

;(display-templates-as-text)

; templates in internal format, with all eight 2D transformations of each template.
(var templates (make-array (list TEMPLATE_COUNT 2D_TRANSFORMATION_COUNT TEMPLATE_WIDTH TEMPLATE_HEIGHT) :initial-element EMPTY_SQUARE))
(var template-stats (make-array TEMPLATE_COUNT :initial-element 0))

(mc template-square (template transformation col row)
  `(aref templates ,template ,transformation ,col ,row))
(mc template-enabled? (template transformation)
  `(template-square ,template ,transformation 0 0)) ; element[0,0] = nil: disabled
(mc disable-template! (template transformation)
  `(set! (template-square ,template ,transformation 0 0) nil))
(mc for-each-template-and-transformation (template transformation &rest body)
  `(dotimes (,template TEMPLATE_COUNT)
     (dotimes (,transformation 2D_TRANSFORMATION_COUNT)
       (do.. ,@body))))

(fn initialize-templates! (&optional disabled-templates discard-duplicates?) ; creates all eight 2D transformations of each template
  (local-fns
    (
      (equal-templates?( template1 transformation1 template2 transformation2)
        (let ((result t))
          (dotimes (col TEMPLATE_WIDTH)
            (if result
                (dotimes (row TEMPLATE_HEIGHT)
                  (if (not-equal? (template-square template1 transformation1 col row)
                                  (template-square template2 transformation2 col row))
                      (set! result nil)))))
          result))

      (discard-duplicate-templates! ()
        (let ((result 0))
          (for-each-template-and-transformation template1 transformation1
            (dotimes (template2 (1+ template1))
              (dotimes (transformation2 2D_TRANSFORMATION_COUNT)
                (if (and (or (< template2 template1)
                             (< transformation2 transformation1))
                         (template-enabled? template2 transformation2)
                         (equal-templates? template1 transformation1 template2 transformation2))
                    (do..
                      ;(display-template template1 transformation1)
                      ;(nl)
                      ;(display-template template2 transformation2)
                      ;(format t "Equal: ~D ~A, ~D ~A~%" template2 transformation2 template1 transformation1)
                      (inc! result)
                      (disable-template! template1 transformation1)
                      ;(if (not-equal? (read-line) "")
                      ;    (return-from find-duplicate-templates nil))
                    )))))
          result))
    )
    (for-each-template-and-transformation template transformation ; create all eight 2D transformations of each template
      (for-each-template-square col row
        (let* ((new-coordinates (calculate-2D-transformation transformation col row TEMPLATE_WIDTH TEMPLATE_HEIGHT))
               (new-col (first new-coordinates))
               (new-row (rest  new-coordinates)))
          (set! (template-square template transformation new-col new-row)
                (char-to-square-value (template-square-as-text template col row))))))

    (if discard-duplicates?
        (discard-duplicate-templates!))

    (dolist (template disabled-templates)
      (dotimes (transformation 2D_TRANSFORMATION_COUNT)
        (disable-template! template transformation)))))

(fn display-template (template transformation)
  (format t "~%~D ~A~%" template (nth transformation 2D_TRANSFORMATIONS))
  (dotimes (row TEMPLATE_HEIGHT)
    (dotimes (col TEMPLATE_WIDTH)
      (display1 (square-value-to-char (template-square template transformation col row))))
    (nl)))

(fn display-templates ()
  (for-each-template-and-transformation template transformation
    (display-template template transformation)))

;(do.. (initialize-templates! DISABLED_TEMPLATES) (display-templates)) (read-line)

; game board and board regions. board regions are 3x3 squares when the templates are 5x5 squares.
(var board (make-array (list (+ 3 MAX_BOARD_WIDTH (* 2 TEMPLATE_WIDTH)) (+ 3 MAX_BOARD_HEIGHT (* 2 TEMPLATE_HEIGHT))) :initial-element WALL)) ; +3: 2 for border, 1 for array indexing past the end.
(var board-width  0)
(var board-height 0)
(var border-squares-count 0)
(var inner-squares-count 0)
(var max-fill-board-region-count 0) ; limit
(var fill-board-region-count 0) ; stats

(struct board-region template transformation col row background) ; background: a copy of the board area covered by a template. the copy is taken before the template is put on the board.
(mc .template       (region) `(board-region-template       ,region)) ; convenience accessors
(mc .transformation (region) `(board-region-transformation ,region))
(mc .col            (region) `(board-region-col            ,region))
(mc .row            (region) `(board-region-row            ,region))
(mc .background     (region) `(board-region-background     ,region))

(const BOARD_REGION_WIDTH  (- TEMPLATE_WIDTH  2)) ; e.g., board regions are 3x3 squares when the templates are 5x5 squares
(const BOARD_REGION_HEIGHT (- TEMPLATE_HEIGHT 2))
(var board-regions (make-array (* (ceiling (array-dimension board 0) BOARD_REGION_WIDTH) (ceiling (array-dimension board 1) BOARD_REGION_HEIGHT)) :fill-pointer 0))
(var board-region-col-count 0)
(var board-region-row-count 0)

(const MAX_FILL_BOARD_REGION_COUNT 10000) ; limit: give up after trying to fill board regions this number of times

(const DEFAULT_BOARD_WIDTH  21)
(const DEFAULT_BOARD_HEIGHT 18)

(mc board-square        (col row)      `(aref board ,col ,row))

(mc box-value?          (value)        `(bit? BOX_BIT                ,value))
(mc empty-value?        (value)        `(zero? (object-bits          ,value)))
(mc empty-floor-value?  (value)        `(equal? FLOOR_VALUE
                                                (object-bits         ,value)))
(mc floor-value?        (value)        `(bit? FLOOR_BIT              ,value))
(mc goal-value?         (value)        `(bit? GOAL_BIT               ,value))
(mc illegal-box-square-value?
                        (value)        `(bit? ILLEGAL_BOX_SQUARE_BIT ,value))
(mc pusher-value?       (value)        `(bit? PUSHER_BIT             ,value))
(mc wall-value?         (value)        `(bit? WALL_BIT               ,value))

(mc floor-square?       (col row)      `(floor-value?       (board-square ,col ,row)))
(mc wall-square?        (col row)      `(wall-value?        (board-square ,col ,row)))
(mc illegal-box-square? (col row)      `(illegal-box-square-value?
                                                            (board-square ,col ,row)))

(fn fill-board-with-value! (board col row width height value)
  (dotimes (col-offset width)
    (dotimes (row-offset height)
      (set! (aref board (+ col col-offset) (+ row row-offset)) value))))

(fn initialize-board! (width height)
  ; make board width and height multiples of the board region width and height respectively, and add a wall-filled border
  (set! board-region-col-count (floor (+ (max (min width  MAX_BOARD_WIDTH ) MIN_BOARD_WIDTH ) BOARD_REGION_WIDTH  -1) BOARD_REGION_WIDTH ))
  (set! board-region-row-count (floor (+ (max (min height MAX_BOARD_HEIGHT) MIN_BOARD_HEIGHT) BOARD_REGION_HEIGHT -1) BOARD_REGION_HEIGHT))
  (set! board-width            (+ 2 (* board-region-col-count BOARD_REGION_WIDTH)))  ; +2: add a wall-filled border
  (set! board-height           (+ 2 (* board-region-row-count BOARD_REGION_HEIGHT))) ; +2: add a wall-filled border
  (set! inner-squares-count    (* (- board-width 2) (- board-height 2))) ; outer wall excluded
  (set! border-squares-count   (- (* board-width board-height) inner-squares-count)) ; outer wall squares
  (fill-board-with-value!      board 0 0 (array-dimension board 0) (array-dimension board 1) WALL) ; fill the entire board with walls
  (fill-board-with-value!      board 1 1 (- board-width 2) (- board-height 2) EMPTY_SQUARE) ; clear the interior of the board. it has a wall-filled border.

  ; make board regions
  (set! (fill-pointer board-regions) 0) ; clear the board regions vector
; (dotimes (row board-region-row-count)
;   (dotimes (col board-region-col-count)
  ; make regions starting from the center of the board and then going round in a spiral.
  ; the board constraints are checked each time a region tentatively is filled with a template.
  ; filling touching regions first helps catching constraint violations like 3x4 floor areas early on.
  (dolist (col-row (reverse! (spiral-order board-region-col-count board-region-row-count)))
    (let ((col (first col-row))
          (row (rest  col-row)))
      (vector-push-extend (make-board-region
                            :col (1+ (* col BOARD_REGION_WIDTH)) ; +1: the board has a wall-filled border
                            :row (1+ (* row BOARD_REGION_HEIGHT))
                            :background (make-array (list TEMPLATE_WIDTH TEMPLATE_HEIGHT) :initial-element WALL))
                          board-regions))))

(fn display-board (&optional a-board stream) ; precondition: if a-board is supplied, then it has the same dimensions as the global board.
  (let ((line (make-string board-width))
        (board (or a-board board))) ; either a board passed as argument to the function, or the global board
    (dotimes (row board-height)
      (dotimes (col board-width)
        (set! (aref line col) (square-value-to-char (aref board col (- board-height row 1)))))
      (if stream
          (do.. (write line :stream stream) (nl stream))
          (displayln line)))))

(fn display-board-regions ()
    (dotimes (index (length board-regions))
      (displayln (aref board-regions index))))

;(do.. (initialize-board! 9 6) (display-board))

(fn copy-board (board)
  (let ((result (make-array (list (array-dimension board 0) (array-dimension board 1)))))
    (dotimes (col (array-dimension board 0))
      (dotimes (row (array-dimension board 1))
        (set! (aref result col row) (aref board col row))))
    result))

(mc neighbor-square-col (col direction)
  `(+ ,col (aref COMPASS_DIRECTIONS_COL ,direction)))
(mc neighbor-square-row (row direction)
  `(+ ,row (aref COMPASS_DIRECTIONS_ROW ,direction)))

(mc neighbor-square-value (col row direction)
  `(board-square (neighbor-square-col ,col ,direction) (neighbor-square-row ,row ,direction)))

(mc for-each-integer-in-series (index count &rest body)
  `(dotimes (,index ,count) ,@body))

(mc for-each-direction (direction &rest body)  `(for-each-integer-in-series ,direction  DIRECTION_COUNT ,@body))

(mc for-each-square (col row &rest body)
  `(dotimes (,row board-height)
     (dotimes (,col board-width)
       ,@body)))

(mc for-each-floor-square (col row &rest body)
  `(for-each-square ,col ,row (if (floor-square? ,col ,row) (do.. ,@body))))

(mc for-each-neighbor-square (col row neighbor-col neighbor-row &rest body) ; precondition: the square is an inner square, not a border square
  (let ((direction (new-symbol)))
    `(let (,neighbor-col ,neighbor-row)
       (for-each-direction ,direction
         (set! ,neighbor-col (neighbor-square-col ,col ,direction))
         (set! ,neighbor-row (neighbor-square-row ,row ,direction))
         ,@body))))

(fn wall-neighbor-count (col row) ; returns the number of neighbor wall squares. precondition: the square is an inner square, not a border square.
  (let ((result 0))
    (for-each-neighbor-square col row neighbor-col neighbor-row
      (if (wall-square? neighbor-col neighbor-row)
          (inc! result)))
    result))

(fn corner-square-types (col row) ; returns the corner types of the specified square. precondition: the square is an inner square, not a border square.
  (let ((result 0))
     (if (floor-square? col row)
         (do.. (if (wall-square? (1- col) row)
                   (do.. (if (wall-square? col (1- row))
                             (inc! result TOP_LEFT))
                         (if (wall-square? col (1+ row))
                             (inc! result BOTTOM_LEFT))))
               (if (wall-square? (1+ col) row)
                   (do.. (if (wall-square? col (1- row))
                             (inc! result TOP_RIGHT))
                         (if (wall-square? col (1+ row))
                             (inc! result BOTTOM_RIGHT))))))
     result))

; puzzle file output
(var puzzle-file-name         nil)
(var puzzle-file-puzzle-count 0)

(fn write-puzzle-to-stream (title board stream)
  (let ((*print-escape* nil))
    (format stream "~%~A~%" title)
    (display-board board stream)))

(fn save-puzzle-to-file (title board file-name)
  (with-open-file (file file-name :direction :output :if-exists :append :if-does-not-exist :create)
    (write-puzzle-to-stream title board file)))

(fn save-puzzle (board)
  (if puzzle-file-name
      (do.. (save-puzzle-to-file (1+ puzzle-file-puzzle-count) board puzzle-file-name)
            (inc! puzzle-file-puzzle-count)) ; +1: assume the puzzle was saved successfully
      (error TEXT_ERROR_NO_PUZZLE_FILE_NAME)))

; board square stack for the function "board-satisfies-constraints?"
(var board-square-stack (make-array (* 2 (array-dimension board 0) (array-dimension board 1)) :fill-pointer 0)) ; *2: for column and row

(mc board-square-stack-clear! ()
  '(set! (fill-pointer board-square-stack) 0))
(mc board-square-stack-empty? ()
  '(zero? (fill-pointer board-square-stack)))
(mc board-square-stack-push! (col row) ; precondition: stack not full
  `(do.. (vector-push ,col board-square-stack)
         (vector-push ,row board-square-stack)))
(mc board-square-stack-pop! (col row) ; precondition: stack  not empty
  `(do.. (set! ,row (aref board-square-stack (dec! (fill-pointer board-square-stack))))
         (set! ,col (aref board-square-stack (dec! (fill-pointer board-square-stack))))))

; timestamp board for the function "board-satisfies-constraints?".
; timestamping avoids initialization of a large "visited?" array before each invocation of the flood fill algorithm which searches for connected floor squares and unfilled squares.
(const         MAX_TIMESTAMP                   (- MOST-POSITIVE-FIXNUM DIRECTION_COUNT 10))
(var timestamp MAX_TIMESTAMP)
(var timestamp-board (make-array (list (array-dimension board 0) (array-dimension board 1)) :initial-element 0))

(mc with-timestamp-board! (&rest body)
  `(do.. (if (>= timestamp MAX_TIMESTAMP)
             (do.. (fill-board-with-value! timestamp-board 0 0 (array-dimension timestamp-board 0) (array-dimension timestamp-board 1) 0)
                   (set! timestamp 0)))
         (inc! timestamp)
         ,@body))

(fn board-satisfies-constraints? ()
  (let ((result                 t)
        (connected-floors-count 0)
        (wall-count             0))
    (local-fns
      ( 
        (visit-access-area (col row) ; preconditions: the specified square is 1) an unvisited square, 2) an inner square, and 3) not wall square
          (let ((result 1)
                square-col
                square-row)
            (set! (aref timestamp-board col row) timestamp) ; mark square as visited
            (board-square-stack-push! col row)
            (while.. (not (board-square-stack-empty?)) ; visit and mark the connected floors and unvisited squares
              (board-square-stack-pop! square-col square-row)
              (for-each-neighbor-square square-col square-row neighbor-col neighbor-row
                (if (not (or (equal? timestamp        (aref timestamp-board neighbor-col neighbor-row))
                             (wall-square?            neighbor-col neighbor-row)
                             (illegal-box-square?     neighbor-col neighbor-row)))
                    (do.. (inc! result)
                          (set! (aref timestamp-board neighbor-col neighbor-row) timestamp) ; timestamped square: visited square
                          (board-square-stack-push!   neighbor-col neighbor-row)))))            
            result))
     
        (4x3-floor-area? (col row) ; returns true if the specified square is the top left square of 4x3 or 3x4 floor area
          (let ((result t))
            (dotimes (col-index 3) ; first check for a 3x3 floor area
              (if result
                  (dotimes (row-index 3)
                    (if (not (floor-square? (min board-width (+ col col-index)) (min board-height (+ row row-index))))
                        (set! result nil)))))
            (if result ; true: found a 3x3 floor area
                (do.. (dotimes (col-index 3) ; check 3 columns x 4 rows
                        (if (not (floor-square? (min board-width (+ col col-index)) (min board-height (+ row 3))))
                            (set! result nil)))
                      (if (not result) ; true: didn't find a 3 columns x 4 rows floor area. check for a 4 columns x 3 rows floor area.
                          (do.. (set! result t)
                                (dotimes (row-index 3) ; check 4 columns x 3 rows
                                  (if (not (floor-square? (min board-width (+ col 3)) (min board-height (+ row row-index))))
                                      (set! result nil))))))) ; nil: not a 4 columns x 3 rows floor area
            result))
     
        (isolated-2xN-floor-area? (col row) 
          ; returns true if the specified square is a left side corner square of a 2xN or Nx2 area
          ; with floors or unfilled squares, and if the area has at the most one floor or unfilled 
          ; square connecting it to the rest of the board 
          
          (local-fns
            (
              (floors-or-unfilled-squares-connecting-floor-area-to-the-rest-of-the-board (left top width height)
                (let ((result 0)
                      (right  (+ left width ))  ; exclusive
                      (bottom (+ top  height))) ; exclusive
                  (dotimes (col-offset width)
                    (if (not (wall-square? (+ col col-offset) (1- top)))
                        (inc! result))
                    (if (not (wall-square? (+ col col-offset) bottom))
                        (inc! result)))
                  (dotimes (row-offset height)
                    (if (not (wall-square? (1- left) (+ top row-offset)))
                        (inc! result))
                    (if (not (wall-square? right     (+ top row-offset)))
                        (inc! result)))
                  result))
            ) 
            (let* ((corner-types (corner-square-types col row))
                   (row-offset (if (not (zero? (bitand TOP_LEFT corner-types)))
                                   (aref COMPASS_DIRECTIONS_ROW DIRECTION_DOWN)
                                   (if (not (zero? (bitand BOTTOM_LEFT corner-types))) 
                                       (aref COMPASS_DIRECTIONS_ROW DIRECTION_UP)   
                                       nil)))) ; nil: not a left side corner square
              (if (and row-offset ; true: a left side corner square
                       (or (> board-width  (+ BOARD_REGION_WIDTH  2)) ; +2: wall border
                           (> board-height (+ BOARD_REGION_HEIGHT 2)))) ; true: the board is big enough to have isolated floor areas
                  (let ((neighbor-row (+ row row-offset))
                        (col-count 0)
                        (row-count 0)
                        connector-count)
                    ; look for an N columns x 2 rows area with floors or unfilled squares, i.e., with no walls
                    (while.. (and (not (wall-square? (+ col col-count) row))
                                  (not (wall-square? (+ col col-count) neighbor-row)))
                             (inc! col-count))
                    (if (and (> col-count 1) 
                             (<= (floors-or-unfilled-squares-connecting-floor-area-to-the-rest-of-the-board col (min row neighbor-row) col-count 2) 1))
                        (do.. ; (display-board) (display "Nx2: " col row col-count) (read-line) 
                              t ; true: the specified square is a left side corner of an isolated N columns x 2 rows area with floors or unfilled squares
                        ) 
                        (do..
                          ; look for a 2 columns x N rows area with floors or unfilled squares, i.e., with no walls  
                          (while.. (and (not (wall-square? col      (+ row (* row-count row-offset))))
                                        (not (wall-square? (1+ col) (+ row (* row-count row-offset)))))
                                   (inc! row-count))
                          (if (> row-count 1)
                              (do.. (set! connector-count (floors-or-unfilled-squares-connecting-floor-area-to-the-rest-of-the-board col (min row (+ row (* (1- row-count) row-offset))) 2 row-count))
                                    (if (<= connector-count 1)
                                        (do.. ; (display-board) (display "2xN: " col row row-count) (read-line)
                                              t ; true: the specified square is a left side corner of an isolated 2 columns x N rows area with floors or unfilled squares
                                        )  
                                        (if (and (= connector-count 2) (= col-count 2) (= 2 row-count))
                                            ; look for a dead-end 2x2 floor area
                                            ; ####
                                            ; #--#
                                            ; #---
                                            ; ##-
                                            (or (zero? (wall-neighbor-count col      row))
                                                (zero? (wall-neighbor-count (1+ col) row))
                                                (zero? (wall-neighbor-count col      neighbor-row))
                                                (zero? (wall-neighbor-count (1+ col) neighbor-row))))))))))))))
      )
      (with-timestamp-board!
        (board-square-stack-clear!)
        (for-each-square col row
          (if (wall-square? col row)
              (inc! wall-count)
              (do.. ; a floor square or an unfilled square

                  ; all floors and unfilled squares must be connected
                  (if (and result 
                           (not-equal? timestamp (aref timestamp-board col row)) ; true: an unvisited floor or unfilled square
                           (not (illegal-box-square? col row))) ; otherwise, it's the special case where a box cannot pass through the square. see the templates.
                      (if (zero? connected-floors-count) ; true: this is the first unvisited area of floors and unfilled squares
                          (inc! connected-floors-count (visit-access-area col row))
                          (set! result nil))) ; found more than one area with floors and unfilled squares

                  ; there must be no 3x4 or 4x3 floor areas
                  (if (and result (4x3-floor-area? col row))
                      (set! result nil))
               
                  ; a floor must not have more than 2 wall neighbors
                  (if (and result
                           (floor-square? col row)
                           (> (wall-neighbor-count col row) 2))
                      (set! result nil))              
               
                  ; there must be no Nx2 or 2xN floor areas with only one floor or unfilled square connecting it to the rest of the board.
                  ; (not a constraint from Taylor's thesis.)               
                  (if (and result (isolated-2xN-floor-area? col row))             
                      (set! result nil)))))))
                      
    ; only 50% of the inner squares may be filled with walls. (not a constraint from Taylor's thesis.)
    (if (> (- wall-count border-squares-count) (/ inner-squares-count 2))
        (set! result nil))

    result))

(fn put-template-on-board! (template transformation col row)
  (let (value)
    (for-each-template-square template-col template-row
      (if (not-equal? EMPTY_SQUARE (set! value (template-square template transformation template-col template-row)))
          (set! (board-square (+ col template-col) (+ row template-row)) value))))
  (inc! fill-board-region-count); ; stats
  (inc! (aref template-stats template)) ; stats
; (display-board) (format t "fill board ~D ~D" col row) (read-line)
)

(mc make-template-candidate (&rest r) `(make-board-region ,@r)) ; for convenience, template candidates use the same structure as board regions

(fn find-template-candidates (col row &optional candidates) ; returns the templates which fit on the board at col, row
  (local-fns
     (
       (find-next-template-candidate (col row candidate)
         (local-fns
           (
            (compatible-square-values? (template-square board-square)
              ;(display "compatible?" template-square board-square) (read-line)
              (or (equal? template-square EMPTY_SQUARE)
                  (equal? board-square    EMPTY_SQUARE)
                  (equal? template-square board-square)))

            (template-fits? (template transformation col row)
              ;(display "template-fits?" template transformation col row) (read-line)
              (if (template-enabled? template transformation)
                  (let ((result t))
                     (dotimes (template-col TEMPLATE_WIDTH)
                       (if result
                           (dotimes (template-row TEMPLATE_HEIGHT)
                             (if result
                                 (set! result (compatible-square-values? (template-square template transformation template-col template-row)
                                                                         (board-square (+ col template-col) (+ row template-row))))))))
                     result)))

            (find-next-template-candidate (template transformation col row)
              ;(display "find-next" template transformation col row) (read-line)
              (if (< template TEMPLATE_COUNT)
                  (if (< transformation 2D_TRANSFORMATION_COUNT)
                      (if (template-fits? template transformation col row)
                          (make-template-candidate :template template :transformation transformation :col col :row row)
                          (find-next-template-candidate template (1+ transformation) col row))
                      (find-next-template-candidate (1+ template) 2DTransformation0 col row)))) ; advance to the next template, starting with the identity transformation
         )

         (if candidate
             (find-next-template-candidate (.template candidate) (1+ (.transformation candidate)) col row) ; try the next [template x transformation]
             (find-next-template-candidate 0 2DTransformation0 col row)))) ; begin the recursion with the first template and the identity transformation
     )

     (let ((candidate (find-next-template-candidate col row (first candidates))))
       (if candidate ; true: found a candidate. collect it and search for more candidates.
           (find-template-candidates col row (cons candidate candidates))
           (reverse! candidates))))) ; reverse!: return the candidates in creation order

(mc save-or-restore-board-region! (board-region save?)
  ; saves or restores the board region, including its surroundings which are affected when a template is put on the board
  `(let ((board-col (1- (.col ,board-region))) ; -1: e.g., when regions are 3x3, the templates are 5x5
         (board-row (1- (.row ,board-region))) ; -1: e.g., when regions are 3x3, the templates are 5x5
         (background (.background ,board-region)))
     (for-each-template-square col row
       (set! ,@(if save?       '((aref background col row)))
             (board-square     (+ col board-col) (+ row board-row))
             ,@(if (not save?) '((aref background col row)))))))

(fn save-board-region! (board-region)
  (save-or-restore-board-region! board-region t))

(fn restore-board-region! (board-region) ; precondition: the board region has previously been saved by calling save-board-region!
  (save-or-restore-board-region! board-region nil))

(fn try-to-fill-board-region! (region-index)
  (if (< region-index (length board-regions)) ; true: board not filled yet. try to fill the selected region.
      (let* ((result nil)
             (board-region (aref board-regions region-index))
             (template-candidates (shuffle-list (find-template-candidates (1- (.col board-region)) (1- (.row board-region))))) ; template candidates in random order
             candidate)
        (if template-candidates ; true: found template candidates for the selected region
            (do.. (save-board-region! board-region)
                  (while.. (and (not result) ; for each template candidate, until the entire board has been filled...
                                template-candidates
                                (< fill-board-region-count max-fill-board-region-count))
                    (set! candidate (pop template-candidates)) ; take the next candidate from the list
                    (put-template-on-board! (.template candidate) (.transformation candidate) (.col candidate) (.row candidate))
                    (if (and (board-satisfies-constraints?)
                             (set! result (try-to-fill-board-region! (1+ region-index)))) ; true: the board has been filled
                        (do.. (set! (.template       board-region) (.template       candidate)) ; register the template which has been used for filling this board region
                              (set! (.transformation board-region) (.transformation candidate)))
                        (restore-board-region! board-region))) ; restore the board region, so the next template candidate can be put on the board
                  result)))
      t)) ; true: all board regions have been filled with templates

(fn generate-board! (width height &optional seed)
  (initialize-board! width height)
; (display-board)
; (format t "width: ~D height: ~D~%" (- board-width 2) (- board-height 2))
; (display-board-regions)

  (set! max-fill-board-region-count MAX_FILL_BOARD_REGION_COUNT) ; limit
  (set! fill-board-region-count 0) ; stats
  (dotimes (template TEMPLATE_COUNT)
    (set! (aref template-stats template) 0)) ; stats

  (set-minimal-standard-random-number-generator-seed! (or seed (get-tick-count))) ; if no seed has been specified then use the current time as seed
  (format t TEXT_FORMAT_WIDTH_HEIGHT_SEED (- board-width 2) (- board-height 2) minimal-standard-random-number-generator-seed)

; randomize the order in which the board regions are filled. this is disabled.
; it's makes the board generator slow, and often it hits the search limit because
; constraint violations cannot be detected as early as they can when touching regions
; are filled first.
; (shuffle-vector! board-regions)
; (display-board-regions)

  (try-to-fill-board-region! 0)); try to fill the board regions recursively, starting with the first region on the queue

; top level functions "initialize", "run", and "finalize"
(const CHAR_COMMAND_MAP '((#\W . width) (#\H . height) (#\R . round) (#\E . seed) (#\S . save) (#\F . filename) (#\Q . quit) (#\newline . continue)))

(fn run (width height &optional seed)
  (let ((result nil)
        (command nil) ; command format: (command . parameter)
        (candidates-per-round 10)
        boards)
    (local-fns
      (
        (read-command ()
          (let ((result (list nil)) ; command format: (command . parameter). the parameter must be nil or a non-empty text string.
                text)
            (while.. (not (first result)) ; until a command has been entered
              (format t TEXT_FORMAT_PROMPT_COMMAND
                        TEXT_PROGRAM_TITLE TEXT_PROGRAM_VERSION candidates-per-round candidates-per-round (- board-width 2) (- board-height 2))
              (set! text (read-line))
              (if (and (string? text)
                       (not (zero? (length text))))
                  (do.. (set! (first result) (rest (assoc (aref (string-upcase text) 0) CHAR_COMMAND_MAP)))
                        (set! text (string-trim " " (subseq text 1))) ; drop leading command character from text. the rest may be a parameter.
                        (if (zero? (length text))
                            (set! (rest result) nil)    ; no parameter text
                            (set! (rest result) text))) ; got parameter text
                  (set! result (list 'continue)))) ; no non-empty text string has been entered. continue with the current settings.
            result))

        (read-parameter (caption parameter integer?) ; precondition: parameter is nil or a text string
          (if (not (string? parameter))
              (do.. (nl)
                    (display caption)
                    (set! parameter (read-line))))
          (if (and (string? parameter)
                   (not (zero? (length (set! parameter (string-trim " " parameter))))))
              (if integer?
                  (parse-integer parameter :junk-allowed t)
                  parameter)))

        (perform-command! (command) ; command format: (command . parameter). precondition: the parameter is nil or a trimmed text string.
          (let ((result nil) ; false: keep asking the user for commands
                (parameter (rest command)))
            (case (first command)

              (continue
                (set! result t)) ; true: stop asking the user for commands

              (filename
               (if (set! parameter (read-parameter
                                      (format nil TEXT_FORMAT_PROMPT_PUZZLE_FILE_NAME (or puzzle-file-name TEXT_NONE))
                                      parameter
                                      nil))
                   (do.. (if (not (equalp parameter puzzle-file-name))
                             (set! puzzle-file-puzzle-count 0)) ; reset counter when changing file
                         (set! puzzle-file-name parameter))))   ; register new filename

              (height
                (if (set! parameter (read-parameter (format nil TEXT_FORMAT_PROMPT_HEIGHT BOARD_REGION_HEIGHT) parameter t))
                    (do.. (initialize-board! (- board-width 2) parameter)
                          (set! height (- board-height 2))))) ; update local height variable

              (quit
                (set! result t)) ; true: stop asking the user for commands

              (round
                (if (set! parameter (read-parameter TEXT_PROMPT_CANDIDATES_PER_ROUND parameter t))
                    (set! candidates-per-round (max 1 (min 1000 parameter)))))

              (save
                (if (or puzzle-file-name (perform-command! (cons 'filename parameter)) puzzle-file-name)
                    (do.. (dolist (board   (reverse boards))
                            (save-puzzle board))
                          (format t TEXT_FORMAT_PUZZLES_SAVED_TO_FILE (length boards) (length boards) puzzle-file-name))))

              (seed
                (if (set! parameter (read-parameter TEXT_PROMPT_RANDOM_SEED parameter t))
                    (set! seed (set-minimal-standard-random-number-generator-seed! parameter))))

              (width
                (if (set! parameter (read-parameter (format nil TEXT_FORMAT_PROMPT_WIDTH BOARD_REGION_WIDTH) parameter t))
                    (do.. (initialize-board! parameter (- board-height 2))
                          (set! width (- board-width 2)))))) ; update local width variable

            result)) ; if false: keep asking the user for commands. if true: stop asking the user for commands.
      )
     
      (if (not seed) ; true: use the current time as random number seed
          (set! seed (set-minimal-standard-random-number-generator-seed! (get-tick-count))))
      (while.. (not-equal? 'quit (first command))  ; until the user terminates the command loop
        (set! boards nil) ; initialize boards generated in this round
        (dotimes (index candidates-per-round)
          (if (set! result (generate-board! width height seed))
              (push (copy-board board) boards)) ; collect the boards generated in this round
          (display-board)
          (format t TEXT_FORMAT_BOARD_GENERATOR_STATISTICS (length board-regions) fill-board-region-count)
          ; prepare the random number seed for generating another board
          (set! seed (set-minimal-standard-random-number-generator-seed! (1+ seed))))

        (while.. (not (perform-command! (set! command (read-command)))))) ; repeat performing commands until "continue" or "quit"
      result)))

(fn initialize ()
  (initialize-templates! DISABLED_TEMPLATES))

(fn finalize ()
  (displayln TEXT_DONE)
  (format t TEXT_BOARD_GENERATOR_INSTRUCTIONS))

(fn main (&optional (width DEFAULT_BOARD_WIDTH) (height DEFAULT_BOARD_HEIGHT) seed)
  (initialize)
  (run width height seed)
  (finalize))

; ______________________________________________________________________________

(in-package "Sokoban")
(main)
;(main 50 50 1)
;(main 21 12 1)
