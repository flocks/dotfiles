(use-package gptel
  :straight (:host github :repo "karthink/gptel" :no-submodules t)
  :config
  (setq gptel-backend (gptel-make-openai "OpenRouter"               
						:host "openrouter.ai"
						:endpoint "/api/v1/chat/completions"
						:stream t
						:key (ft-extract-auth-token "openrouter.ai" "flocks")
						:models '(openai/gpt-3.5-turbo
								  mistralai/mixtral-8x7b-instruct
								  meta-llama/codellama-34b-instruct
								  codellama/codellama-70b-instruct
								  google/palm-2-codechat-bison-32k
								  google/gemini-pro))))

(provide 'ft-gpt)
