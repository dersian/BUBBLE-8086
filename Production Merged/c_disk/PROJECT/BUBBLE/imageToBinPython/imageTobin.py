from PIL import Image
import io

#---Steps---#
#1. Resize .png image to 320x200 for background or desired aspect ratio for other elements
#2. Decrease color depth to 4bit -> 16 colors -- we use 8bit color palette -> 256 colors
#3. Find unique rgb color scheme palette
#4. Copy rgb codes to palette array
#5. Adjust offset
#6. Adjust x and y values
#7. Copy bin file to .asm directory
#8. Copy color palette output to .asm file

def printColors6(colorList):
    colors6 = []
    for color in colorList:
        colors6.append((int(color[0]/4), int(color[1]/4), int(color[2]/4)))
    for color in colors6:
        print("db "+str(color[0])+","+str(color[1])+","+str(color[2]))
        
def printColorSpace(amount):
    colorsSpace = []
    for i in range(amount):
        print("db 0,0,0")

#background
with Image.open('Images/Background/bg_R_4bit.png') as im:
    px = im.load()
    pixels=[]
    with open("Images/bg.bin","wb") as file:
        for y in range(200):
            for x in range(320):
                pixels.append(px[x,y])
                file.write(px[x,y].to_bytes(1, byteorder='big', signed=False))

#bg_8bitpalette = [(2,38,54),(148,169,175),(20,98,144),(236,239,236)]
bg_8bitpalette = [(37,34,55),(201,150,109),(121,60,128),(151,97,91),(77,51,92),(108,112,149),(160,76,150),(93,66,101),(114,68,57),(231,212,170),(175,154,197),(248,249,251),(204,112,192),(184,112,82),(188,183,208),(224,183,132)] # Bubbles background
printColors6(bg_8bitpalette)

#blue ball
with Image.open('Images/Assets/blueball_4bit.png') as im:
    px = im.load()
    pixels=[]
    with open("Images/blueball.bin","wb") as file:
        for y in range(8):
            for x in range(8):
                pixels.append(px[x,y]+16)
                file.write((px[x,y]+16).to_bytes(1, byteorder='big', signed=False))
        #print(pixels)

#blueball_8bitpalette = [(7,23,45),(19,139,227),(11,83,168),(86,210,246)]
blueball_8bitpalette = [(7,10,14),(7,126,223),(7,59,147),(63,185,239),(7,84,173),(7,38,85),(45,167,235),(31,109,182),(7,52,100),(96,220,249),(23,81,119),(7,107,203),(7,95,191),(19,141,227),(74,198,242),(7,15,28)] # 16 color blue ball
printColors6(blueball_8bitpalette)
#printColorSpace(12)

#green ball
with Image.open('Images/Assets/greenball_4bit.png') as im:
    px = im.load()
    pixels=[]
    with open("Images/greenball.bin","wb") as file:
        for y in range(8):
            for x in range(8):
                pixels.append(px[x,y]+32)
                file.write((px[x,y]+32).to_bytes(1, byteorder='big', signed=False))
        #print(pixels)
                
#greenball_8bitpalette = [(54,26,7),(227,139,19),(172,86,12),(246,210,86)]
greenball_8bitpalette = [(7,14,8),(7,137,10),(31,203,7),(127,241,70),(44,223,9),(7,79,8),(93,235,45),(20,173,7),(8,28,7),(16,103,7),(23,191,7),(160,249,96),(43,119,23),(51,182,31),(11,92,7),(7,152,10)] # 16 color green ball
printColors6(greenball_8bitpalette)
#printColorSpace(12)


#yellow ball
with Image.open('Images/Assets/yellowball_4bit.png') as im:
    px = im.load()
    pixels=[]
    with open("Images/yellowball.bin","wb") as file:
        for y in range(8):
            for x in range(8):
                pixels.append(px[x,y]+48)
                file.write((px[x,y]+48).to_bytes(1, byteorder='big', signed=False))

#yellowball_8bitpalette = [(17,18,7),(139,158,9),(225,214,34),(86,94,9)]
yellowball_8bitpalette = [(18,12,7),(223,126,7),(152,61,7),(239,185,63),(87,39,7),(194,98,7),(235,167,45),(115,53,7),(77,33,7),(249,220,96),(119,81,23),(206,110,7),(173,84,7),(227,141,19),(182,109,31),(242,198,74)] # 16 color yellow ball
printColors6(yellowball_8bitpalette)

#pink ball
with Image.open('Images/Assets/pinkball_4bit.png') as im:
    px = im.load()
    pixels=[]
    with open("Images/pinkball.bin","wb") as file:
        for y in range(8):
            for x in range(8):
                pixels.append(px[x,y]+64)
                file.write((px[x,y]+64).to_bytes(1, byteorder='big', signed=False))

#pinkball_8bitpalette = [(35,7,24),(219,18,173),(244,86,241),(140,8,92)]
pinkball_8bitpalette = [(18,7,14),(198,7,141),(239,63,221),(100,7,71),(225,7,173),(207,7,153),(119,23,101),(85,7,88),(147,7,88),(235,45,206),(246,96,249),(173,7,118),(182,31,140),(242,74,223),(227,19,185),(222,7,169)] # 16 color pink ball
printColors6(pinkball_8bitpalette)
#printColorSpace(12)
