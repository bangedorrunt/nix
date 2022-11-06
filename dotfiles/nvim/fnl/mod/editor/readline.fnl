(import-macros {: nmap} :core.macros)

(local {: forward_word
        : backward_word
        : beginning_of_line
        : end_of_line
        : kill_word
        : backward_kill_word
        : unix_word_rubout
        : kill_line
        : backward_kill_line} (require :readline))

(fn setup []
  (nmap "!" :<M-f> forward_word)
  (nmap "!" :<M-b> backward_word)
  (nmap "!" :<C-a> beginning_of_line)
  (nmap "!" :<C-e> end_of_line)
  (nmap "!" :<C-f> :<Right>)
  (nmap "!" :<C-b> :<Left>)
  (nmap "!" :<C-d> :<Delete>)
  (nmap "!" :<C-h> :<BS>)
  (nmap "!" :<M-d> kill_word)
  (nmap "!" :<M-BS> backward_kill_word)
  (nmap "!" :<C-w> unix_word_rubout)
  (nmap "!" :<C-k> kill_line)
  (nmap "!" :<C-u> backward_kill_line))
{: setup}
