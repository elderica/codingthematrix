(in-package :cl-user)
(defpackage codingthematrix/inverse-index
  (:use :cl))
(in-package :codingthematrix/inverse-index)

;;;; 行列プログラマ P.44

;;; 補助関数や変数

;; ハッシュテーブルの中身を表示させる
;; https://stackoverflow.com/questions/22898967/common-lisp-library-for-pretty-printing-e-g-pretty-print-a-nested-hash-table
(defun print-hash-table (hash-table)
  (format t "#HASH{~{~{(~S . ~S)~}~^ ~}}"
          (loop for key being the hash-keys of hash-table
                  using (hash-value value)
                collect (list key value))))

;; 読み込むファイルへのパス
(defparameter *path-to-stories-small*
  (asdf:system-relative-pathname :codingthematrix "resources/stories_small.txt"))

;; ファイルの読み込み
(defparameter *stories-small*
  (with-open-file (file *path-to-stories-small*)
    (loop for line = (read-line file nil)
          while line
          collect line)))

;; 処理対象とする行シーケンス
(defparameter *stories* *stories-small*)

;;; 1行に1つの文書を記述した文書ファイルから、指定された単語を含む文書を特定できるようにするデータ構造を逆インデックスと呼ぼう。

;; 課題0.6.6
;; 単語が与えられると、その単語が現れる文書の文書番号から成るリストに写像するハッシュテーブルを返す関数を作成しよう。
;; このハッシュテーブルを逆インデックスと呼ぶ。

;;(print-hash-table
;; (make-inverse-index
;;  (list "hello world" "hello" "hello cat" "hellolot of cats")))
;; => #HASH{("cat" . (2)) ("hello" . (0 1 2)) ("cats" . (3)) ("world" . (0)) ("hellolot" . (3)) ("of" . (3))}
(defun make-wordset (strlist)
  (loop
     with words = nil
     for str in strlist
     do (setf words
              (union words
                     (remove-duplicates
                      (uiop:split-string str :separator " "))
                     :test #'equal))
     finally (return words)))

(defun make-inverse-index (strlist)
  (let ((words (make-wordset strlist)))
    (loop
       with inverse-index = (make-hash-table :test #'equal)
       for word in words
       do (setf (gethash word inverse-index)
                (remove-duplicates
                 (loop 
                    for str in strlist
                    and index from 0
                    if (find word (uiop:split-string str :separator " ") :test 'equal)
                      collect index)))
       finally (return inverse-index))))

;; 課題0.6.7
;; 逆インデックスinverse-indexと単語のリストqueriesを受けとり、
;; queries内の単語のどれかが含まれている行のインデックスの集合を返す関数を作成しよう。
