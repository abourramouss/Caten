
(in-package :caten/test-suite)

(python-exec "
def sentencepiece_tokenize(str):
    import sentencepiece as spm
    sp = spm.SentencePieceProcessor()
    sp.Load('./source/test-suite/external/assets/llama2_tokenizer/tokenizer.model')
    return sp.EncodeAsIds(str, add_bos=True, add_eos=True)

def sentencepiece_decode(list):
    import sentencepiece as spm
    sp = spm.SentencePieceProcessor()
    sp.Load('./source/test-suite/external/assets/llama2_tokenizer/tokenizer.model')
    return sp.DecodeIds(list)
")
(import-function "sentencepiece_tokenize")
(import-function "sentencepiece_decode")

;; Create our tokenizer instance
(defun make-bpe-llama2 ()
  (make-bpe-tokenizer-llama2))

(defparameter *llama2-tokenizer* (make-bpe-llama2))

(deftest llama2-tokenizer-test
  (macrolet ((compare (sentence)
               `(progn
                  ;; Test encoding
                  (let ((x1 (encode *llama2-tokenizer* ,sentence))
                        (x2 (coerce (sentencepiece_tokenize ,sentence) 'list)))
                    (ok (equal x1 x2) 
                        (format nil "Encoding: ~a~%Ours = ~a~%SentencePiece = ~a" 
                                ,sentence x1 x2)))
                  ;; Test decoding
                  (let ((x1 (decode *llama2-tokenizer* (encode *llama2-tokenizer* ,sentence)))
                        (x2 (sentencepiece_decode (sentencepiece_tokenize ,sentence))))
                    (ok (equal x1 x2)
                        (format nil "Decoding: ~a~%Ours = ~a~%SentencePiece = ~a" 
                                ,sentence x1 x2))))))
    (compare "this"))) ; Newline handling


(run-test 'llama2-tokenizer-test)


