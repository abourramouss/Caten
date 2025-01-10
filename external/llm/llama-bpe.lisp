(in-package :caten/llm)

(defclass LLaMA2Tokenizer (BPETokenizer)
  ()
  (:documentation "LLaMA 2 tokenizer extends BPE with SentencePiece-style word boundaries"))


(defun make-bpe-tokenizer-llama2 ()
  (let* ((tokens-path "./source/test-suite/external/assets/llama2_tokenizer/tokens.txt")
         (merges-path "./source/test-suite/external/assets/llama2_tokenizer/merges.txt")
         (tokens (uiop:read-file-string tokens-path))
         (merges (uiop:read-file-string merges-path)))
    (format t "Tokens file exists? ~A~%" (probe-file tokens-path))
    (format t "Merges file exists? ~A~%" (probe-file merges-path))
    (format t "Tokens content length: ~A~%" (length tokens))
    (format t "Merges content length: ~A~%" (length merges))
    
    (let ((tokenizer (make-bpe-tokenizer tokens merges :from-file-p t)))
      (change-class tokenizer 'LLaMA2Tokenizer)
      tokenizer)))
(defparameter *llama2* (make-bpe-tokenizer-llama2))

(print (encode *llama2* "this")))
