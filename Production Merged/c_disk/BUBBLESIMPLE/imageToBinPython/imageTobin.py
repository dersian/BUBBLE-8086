from PIL import Image
import io

#---Steps---#
#1. Resize .png image to 320x200 for background or desired aspect ratio for other elements
#2. Decrease color depth to 4bit
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
with Image.open('Images/Background/bg_4bit.png') as im:
    px = im.load()
    pixels=[]
    with open("Images/bg.bin","wb") as file:
        for y in range(200):
            for x in range(320):
                pixels.append(px[x,y])
                file.write(px[x,y].to_bytes(1, byteorder='big', signed=False))

bg_8bitpalette = [(2,38,54),(148,169,175),(20,98,144),(236,239,236)]
printColors6(bg_8bitpalette)

#blue ball
with Image.open('Images/Assets/blueball_4bit.png') as im:
    px = im.load()
    pixels=[]
    with open("Images/blueball.bin","wb") as file:
        for y in range(8):
            for x in range(8):
                pixels.append(px[x,y]+4)
                file.write((px[x,y]+4).to_bytes(1, byteorder='big', signed=False))
        #print(pixels)

blueball_8bitpalette = [(7,23,45),(19,139,227),(11,83,168),(86,210,246)]
printColors6(blueball_8bitpalette)
printColorSpace(12)

#green ball
with Image.open('Images/Assets/greenball_4bit.png') as im:
    px = im.load()
    pixels=[]
    with open("Images/greenball.bin","wb") as file:
        for y in range(8):
            for x in range(8):
                pixels.append(px[x,y]+16)
                file.write((px[x,y]+16).to_bytes(1, byteorder='big', signed=False))
        #print(pixels)
                
greenball_8bitpalette = [(54,26,7),(227,139,19),(172,86,12),(246,210,86)]
printColors6(greenball_8bitpalette)
printColorSpace(12)

#pink ball
with Image.open('Images/Assets/pinkball_4bit.png') as im:
    px = im.load()
    pixels=[]
    with open("Images/pinkball.bin","wb") as file:
        for y in range(8):
            for x in range(8):
                pixels.append(px[x,y]+32)
                file.write((px[x,y]+32).to_bytes(1, byteorder='big', signed=False))

pinkball_8bitpalette = [(35,7,24),(219,18,173),(244,86,241),(140,8,92)]
printColors6(pinkball_8bitpalette)
printColorSpace(12)

#yellow ball
with Image.open('Images/Assets/yellowball_4bit.png') as im:
    px = im.load()
    pixels=[]
    with open("Images/yellowball.bin","wb") as file:
        for y in range(8):
            for x in range(8):
                pixels.append(px[x,y]+48)
                file.write((px[x,y]+48).to_bytes(1, byteorder='big', signed=False))

yellowball_8bitpalette = [(17,18,7),(139,158,9),(225,214,34),(86,94,9)]
printColors6(yellowball_8bitpalette)
