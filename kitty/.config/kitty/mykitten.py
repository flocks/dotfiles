def main(args):
   # this is the main entry point of the kitten, it will be executed in
   # the overlay window when the kitten is launched
   answer = input('Enter some text: ')
   # whatever this function returns will be available in the
   # handle_result() function
   return answer

def handle_result(args, answer, target_window_id, boss):
   # get the kitty window into which to paste answer
   w = boss.window_id_map.get(target_window_id)
   if w is not None:
      w.paste(answer)
