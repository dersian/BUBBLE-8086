from PIL import Image
import io

#---Steps---#
#1. Resize .png image to 320x200 for background or desired aspect ratio for other elements
#2. Decrease color depth to 4bit -> 64 colors
#3. Find unique rgb color scheme palette
#4. Copy rgb codes to palette array
#5. Adjust offset
#6. Adjust x and y values
#7. Copy bin file to .asm directory
#8. Copy color palette output to .asm file

def convertTo6bits(palette_8bit):
    palette_6bit = []
    for rgb_row in palette_8bit:
        palette_6bit.append((int(rgb_row[0]/4), int(rgb_row[1]/4), int(rgb_row[2]/4))) # divide by 4: 256 (8 bit palette) -> 64 (6 bit palette)
    for rgb_row in palette_6bit:
        print("db " + str(rgb_row[0]) + "," + str(rgb_row[1]) + "," + str(rgb_row[2]))
        
#background
with Image.open('Images/Background/bg_R_4bit.png') as im:
    im = im.load()
    with open("Images/bg.bin","wb") as file:
        for y in range(200):
            for x in range(320):
                file.write(im[x,y].to_bytes(1, byteorder='big', signed=False))

bg_8bitpalette = [(37,34,55),(201,150,109),(121,60,128),(151,97,91),(77,51,92),(108,112,149),(160,76,150),(93,66,101),(114,68,57),(231,212,170),(175,154,197),(248,249,251),(204,112,192),(184,112,82),(188,183,208),(224,183,132)] # Bubbles background
convertTo6bits(bg_8bitpalette)

#blue ball
with Image.open('Images/Assets/blueball_4bit.png') as im:
    im = im.load()
    with open("Images/blueball.bin","wb") as file:
        for y in range(8):
            for x in range(8):
                file.write((im[x,y]+16).to_bytes(1, byteorder='big', signed=False))

blueball_8bitpalette = [(7,10,14),(7,126,223),(7,59,147),(63,185,239),(7,84,173),(7,38,85),(45,167,235),(31,109,182),(7,52,100),(96,220,249),(23,81,119),(7,107,203),(7,95,191),(19,141,227),(74,198,242),(7,15,28)] # 16 color blue ball
convertTo6bits(blueball_8bitpalette)

#green ball
with Image.open('Images/Assets/greenball_4bit.png') as im:
    im = im.load()
    with open("Images/grball.bin","wb") as file:
        for y in range(8):
            for x in range(8):
                file.write((im[x,y]+32).to_bytes(1, byteorder='big', signed=False))
                
greenball_8bitpalette = [(7,14,8),(7,137,10),(31,203,7),(127,241,70),(44,223,9),(7,79,8),(93,235,45),(20,173,7),(8,28,7),(16,103,7),(23,191,7),(160,249,96),(43,119,23),(51,182,31),(11,92,7),(7,152,10)] # 16 color green ball
convertTo6bits(greenball_8bitpalette)


#yellow ball
with Image.open('Images/Assets/yellowball_4bit.png') as im:
    im = im.load()
    with open("Images/ywball.bin","wb") as file:
        for y in range(8):
            for x in range(8):
                file.write((im[x,y]+48).to_bytes(1, byteorder='big', signed=False))

yellowball_8bitpalette = [(18,12,7),(223,126,7),(152,61,7),(239,185,63),(87,39,7),(194,98,7),(235,167,45),(115,53,7),(77,33,7),(249,220,96),(119,81,23),(206,110,7),(173,84,7),(227,141,19),(182,109,31),(242,198,74)] # 16 color yellow ball
convertTo6bits(yellowball_8bitpalette)

#pink ball
with Image.open('Images/Assets/pinkball_4bit.png') as im:
    im = im.load()
    with open("Images/pinkball.bin","wb") as file:
        for y in range(8):
            for x in range(8):
                file.write((im[x,y]+64).to_bytes(1, byteorder='big', signed=False))

pinkball_8bitpalette = [(18,7,14),(198,7,141),(239,63,221),(100,7,71),(225,7,173),(207,7,153),(119,23,101),(85,7,88),(147,7,88),(235,45,206),(246,96,249),(173,7,118),(182,31,140),(242,74,223),(227,19,185),(222,7,169)] # 16 color pink ball
convertTo6bits(pinkball_8bitpalette)
