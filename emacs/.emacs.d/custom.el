(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application eshell)
	  eshell-connection-default-profile)
	 ((:application tramp :protocol "flatpak")
	  tramp-container-connection-local-default-flatpak-profile)
	 ((:application tramp)
	  tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile
	  (eshell-path-env-list))
	 (tramp-container-connection-local-default-flatpak-profile
	  (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin"))
	 (tramp-connection-local-darwin-ps-profile
	  (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
	  (tramp-process-attributes-ps-format
	   (pid . number)
	   (euid . number)
	   (user . string)
	   (egid . number)
	   (comm . 52)
	   (state . 5)
	   (ppid . number)
	   (pgrp . number)
	   (sess . number)
	   (ttname . string)
	   (tpgid . number)
	   (minflt . number)
	   (majflt . number)
	   (time . tramp-ps-time)
	   (pri . number)
	   (nice . number)
	   (vsize . number)
	   (rss . number)
	   (etime . tramp-ps-time)
	   (pcpu . number)
	   (pmem . number)
	   (args)))
	 (tramp-connection-local-busybox-ps-profile
	  (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
	  (tramp-process-attributes-ps-format
	   (pid . number)
	   (user . string)
	   (group . string)
	   (comm . 52)
	   (state . 5)
	   (ppid . number)
	   (pgrp . number)
	   (ttname . string)
	   (time . tramp-ps-time)
	   (nice . number)
	   (etime . tramp-ps-time)
	   (args)))
	 (tramp-connection-local-bsd-ps-profile
	  (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
	  (tramp-process-attributes-ps-format
	   (pid . number)
	   (euid . number)
	   (user . string)
	   (egid . number)
	   (group . string)
	   (comm . 52)
	   (state . string)
	   (ppid . number)
	   (pgrp . number)
	   (sess . number)
	   (ttname . string)
	   (tpgid . number)
	   (minflt . number)
	   (majflt . number)
	   (time . tramp-ps-time)
	   (pri . number)
	   (nice . number)
	   (vsize . number)
	   (rss . number)
	   (etime . number)
	   (pcpu . number)
	   (pmem . number)
	   (args)))
	 (tramp-connection-local-default-shell-profile
	  (shell-file-name . "/bin/sh")
	  (shell-command-switch . "-c"))
	 (tramp-connection-local-default-system-profile
	  (path-separator . ":")
	  (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("33876ef6caf9c4cb35f8bc9430fb64ba503036a4e80532bb764edec121900248" "e16cd3e6093cf35e65dbac6c7649931936f32f56be34477cb7cbe1ee332c5b99" "e4013fb0675182b04aa22f29054842db418d5a72da7ebb3c5b9dbb624502aaec" "eb7cd622a0916358a6ef6305e661c6abfad4decb4a7c12e73d6df871b8a195f8" "11873c4fbf465b956889adfa9182495db3bf214d9a70c0f858f07f6cc91cbd47" "84c2c93ce268699838b51eeeaaec46e064d5048e232b39113b73209be3ef6fd4" "27fc6293ab3eaafd57703df8552d4e1629649a0f2266f107d8c9157956ce4d4b" "1dbbf33c64f2fa781b0f68494c0edee535d3a9f0c7e106b047d21c719c779a30" "1479377d38b49d40dba561b2e4ab354b78cedb721bd62e0210ca18652760d219" "4d16802de4686030ed8f30b5a844713d68edec9cc07322bef54493d15e68d8cd" "6454421996f0508c38215a633256e36c19a28591542fb0946cfc40f1dceb89cf" "e9aa348abd3713a75f2c5ba279aa581b1c6ec187ebefbfa33373083ff8004c7c" "3ab376acffab6b4e79ae2b6e0a1cce3fa21dbac0027f0ff0dfef02b5c838dba9" "99cacbd48c5e315930dae7144a9b9d285b58d9ebd06b48dcc3b62f11dc051ffe" "7c9da9df5b0b8c56be09a9a41ea5218d0fddc9019db773e5167cad31c6dd13c9" "b00f762e269f7de455651384e13dadb584f7d366d3801354269f0ce4131af8dd" "ae7ffd67c75705c76dcfd66fa212abdcdd62ab5be2635371b31dc4de13f40668" "e405944a54b99c11463edddeb9ff4f8dd981cd2ae14a4b58458536c451323381" "7e068da4ba88162324d9773ec066d93c447c76e9f4ae711ddd0c5d3863489c52" "e3b2bad7b781a968692759ad12cb6552bc39d7057762eefaf168dbe604ce3a4b" "25c9807d70ac15057821f8b830ac93a80ca2b2beca6f1e9e7f77cbc5578e21b4" "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "8a98116c38a1455f30b7963f771f152e6ca4979f1dba44d7d924c684846ef3c7" "83ce78add4890eed936e7e3bf66cb74d4cf583310c3ef15e46c001ccae001050" "0bc864218de42740db1822ab36e138d8d182fff40aec728c18ba690ed5d9d3f8" "3552bb109b685cddd3ddf039bbdabd52595bc1ab2173d5e73e490c80eea47029" "8aa77197f9e81285d348ef2ada129df45a2e690b745afb79a106b1096857e4b6" "800b7247f1db93d1821f243ecd0417fd9a620e805a476be7b262ba7f8d0ff1fd" "7a6cccfe4ff6a7fc5da04b56438732d46133af8b9d349fde9c025aead36e0d07" "64bf4ceacb3986ef96e7e2730c4f947b086d5b62ba19d8b96a89325c5660fed9" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "eff6aabe4be18b849c15481fba01a209550330546573542f7e3c2234fd7d7412" "a56e8b3325891ea0c34f7c88b2978d29533b679a8a87cfaa4a337a818db8002d" "c76f7055d55fb546147f2967cd659471b7c29e1acbc480ce15fbe0829b60adeb" "2a3da9d4f818158dbf2528d0db2530cbf8c62f6b425e008d18a6eff0888c1d26" "5809b3c3c6f71ce0a5bae1bf84a4cbbd85a562b53460392c36b79f8bed60b3b8" "24e856ac77923f7b05905e6c5e4384a61071539b31e10f296bc145ad775bef93" "2fe72e320104693d3d118e6c6d349d64d1e4a2c90d4ce0ac055ab3695f7e6544" "e3743b74d4763f7c645ecb77ad8c8c39eb36e1609a7a7d63dfe9ff78e0d61f47" "16463aca7ca5d861e03a6a5b0c0b2930492af491de081a9ab50f09d7fbbf7ef3" "279f74e365ba5aade8bc702e0588f0c90b5dee6cf04cf61f9455661700a6ebeb" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "38e3a5e4f2ab8169ba4219e5fcd15d0b07067a759abf8a15525f17d87aa526f5" "1cae4424345f7fe5225724301ef1a793e610ae5a4e23c023076dc334a9eb940a" "5a1fd979d77df7a628a6d7c3770ca7a6086b132da7a655d5c7864b05be56dde1" "b6d5e39149c9b3f3f8108e0bda88db49408a9058af2813e11416752279f2c426" "ab64fe30895c281c0a1e4bb091d1038f903f520dec3e947aa16a06503ff93879" "22fea3a596de3d8558bd1ceb5344a5ec1df20abc7c906677180a59bd737b5a51" "48440b79470f7ea212375737b8da9b9c34b95c0c981bbf0d83c99c1cb4665c24" "9d70d35ce321f20bb1dfbc9e6dcfeaaefb233fd1b9782c393aa20063d43d2091" "b1a691bb67bd8bd85b76998caf2386c9a7b2ac98a116534071364ed6489b695d" "5b5adc8b44a8a79e671b4c12c39f2640e8fe3cab473ee51456f33fccea05f7ef" "94422cceb6b73ec604f974fd87bd526131ae3701879f03bde2bd8a047208795e" "8d8207a39e18e2cc95ebddf62f841442d36fcba01a2a9451773d4ed30b632443" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "5b9a45080feaedc7820894ebbfe4f8251e13b66654ac4394cb416fef9fdca789" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "0c08a5c3c2a72e3ca806a29302ef942335292a80c2934c1123e8c732bb2ddd77" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "251ed7ecd97af314cd77b07359a09da12dcd97be35e3ab761d4a92d8d8cf9a71" "512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "adaf421037f4ae6725aa9f5654a2ed49e2cd2765f71e19a7d26a454491b486eb" "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" "169272064635ec628fe5c43853862e2a1d87e19702665bac941d4298dd19c5fd" "3fe1ebb870cc8a28e69763dde7b08c0f6b7e71cc310ffc3394622e5df6e4f0da" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "bf948e3f55a8cd1f420373410911d0a50be5a04a8886cabe8d8e471ad8fdba8e" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "7435c097e5d051fa34ad58bf94ac1b2d5d1e14e62fd20dfedf2bb4403b09a446" "8566e9107b01de614891b9b397c6f5b66ce2d328fd8f04770260c58c62c0c2b8" "3a7f53a0268877716d309d37ea952533391420bf139c38f69752d1494341151a" "ef8c33cc2730745d62d08ae0e5fedd018bb70129a4ec75f942e6d7996252295c" "7360aba98b0e94dd1499fcba50534dd42976dfd50a25f5cf122e6b8f3e53c511" "fefb102e277edbf63698d3994ec8b89ed1d75711a302ea05f095297f0e71f353" "3199be8536de4a8300eaf9ce6d864a35aa802088c0925e944e2b74a574c68fd0" "72ed8b6bffe0bfa8d097810649fd57d2b598deef47c992920aef8b5d9599eefe" "d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" "88f19534c13fe8a05c33bc03852f6d586d2bb5292dd2f93bc8f99727f71b4467" "ae0fcd3896dc3f937483234a7874818fdeeca7a919ecb5a5ab7b5563d9f9c957" "0e44755893f9ff825802c493cded385b1839165c4f094bad72a95125236a5797" "ce409d3a932171747b7b8b7edafdc70b4614beb31a7204aa25517d7c0ab80c48" "01f52ed4dc9cfd4f397eda57c9eb5fea360bd6c18a2684121cc47279bfca5a51" "183dfa34e360f5bc2ee4a6b3f4236e6664f4cfce40de1d43c984e0e8fc5b51ae" "22c213e81a533c259127302ef1e0f2d1f332df83969a1f9cf6d5696cbe789543" "9dbd2c6f93cc1774c261f23042be8bf7decc8f6599c21189c04d7791231b2b79" "931ee45708e894d5233fc4a94ae0065c765c1a0aeb1bd8d9feee22f5622f44b4" "d2b7abf3fb8e9505a478a04bd6d727a029cab49d58c0fafe271293d095438067" "5ca9d0a5971e42ecee31398533e5b9dfc01c61a69bf3fd69395aa189c792252e" "c01cd0485ce35cf0a19ab91146d2c2b6528ec60ad4c8ffec5b2b7cc4bc05bd80" "4e7672ce1015731d9d6955652f8f1b438420f109d15f662a014fa4e992429b9a" "45611797b789abf53e97c43b29c7f10dd6f18971e238e700bc885e702531053a" "935cd704a3b4b12c9c0582da1d25437e2802d0f82c5d46de0eb5a968dfad08da" "3680500faf3f5707fdf16c123c32c74a44c388318e5054bb7ef8b7025158f469" "046e442b73846ae114d575a51be9edb081a1ef29c05ae5e237d5769ecfd70c2e" "0657bbbdf081bafc0b56d4d40cb333829eb576c8c519c8028f29afbf9559eb35" "347313c47366c3cb305fb63dff7df87426061d5529a86c215086fe8581228733" "b330e75c3717166db3a8a41eb180c705b46b97c584dffdb917b310f86025c811" "9ae0e81ced7c8d587cb1db9fcb528856315d352822082518884e8726fe681d1d" "ebcb78e41027f498a1ee53cd4ae41ebbf4c44841fe0ed787affd01ba5013c66b" "9b7d703afa15e320af9aa6b3e02bb1ebb8657c23043f7054357f86ce014b5461" "d983956f195a1a5e920c78216e6c164f4fd73a4fdce3f532d527182409f96515" "83fc7ca4d73cee934d2a96d5f2f6fe243afede055497528ab89dbb92c1145020" default))
 '(dashub--notify-delay 60)
 '(safe-local-variable-values
   '((vc-prepare-patches-separately)
	 (vc-git-annotate-switches . "-w")
	 (diff-add-log-use-relative-names . t)))
 '(smtpmail-smtp-server "smtp.gmail.com" t)
 '(smtpmail-smtp-service 587 t)
 '(tab-bar-show nil)
 '(tab-width 4)
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
