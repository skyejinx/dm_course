#303 
#https://www.google.com/imgres?imgurl=http://www.portauthority.org/paac/portals/0/images/sysmapthumb.png&imgrefurl=http://www.portauthority.org/paac/schedulesMaps/Maps.aspx&h=1311&w=1638&tbnid=dgk_x-KgOCeSSM:&docid=7hU4lUYRWK_JPM&ei=eWvoVsbgOMTLeqb0sYAH&tbm=isch&ved=0ahUKEwiGgdWPv8PLAhXEpR4KHSZ6DHAQMwgdKAEwAQ#h=1311&w=1638
#http://transitmap.net/post/64965179305/allegheny-county-system-map

import PIL.Image
from PIL import ImageTk
from Tkinter import *

root = Tk()

image = PIL.Image.open("sysmapthumb.png")
display = ImageTk.PhotoImage(image)

label = Label(root, image=diplay)
lable.pack()

root.mainloop()
