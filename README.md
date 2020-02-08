# Colonies
Create funky cellular automata. Explore number theory. Make art. Create psychedelic icons. Help me learn good Julia!

Quick Start:

using Pkg; Pkg.add("Colonies") # THIS DOES NOT WORK YET!!!
using Colonies

generatemany(10,10,50,50,true,limit=100) # To create 100 images based on random pairs of a 3x3 mask with a filter.
# Output will appear as PNG files in "img/nnnnn/repeat|regular" below the working directory
# This would create ~ 500,000 files if left to run unlimited. But limits are good. So do this:

-- OR --

generatemany(15,15,100,100,false,limit=50,seed=seedwith(randedge))
# The randedge option introduces a one-pixel thick edge of randomly generated 0:1 cells on 
top of an all white canvas. Think "random picture frame".

-- OR --
redraw("a_really_cool_Colony_file.png","resultdir",20,20,50,50)
# Regenerate the given file, putting it into a possibly non-existant directory,
# The new dimensions are 20 cells x 50 pixels/cell = 1000 pixels in each direction (x and y)
# The resulting image will be a 1000 x 1000 grayscale PNG.

-- OR --
redraw("a_really_cool_Colony_file.png","videodir",layout=VideoLayout(12))
# Regenerate the given cool file (which is required to have been created in Colonies) as an MPEG 4 grayscale video 
# with framerate = 12 fps, putting the result file into a new directory, "resultdir". The filename will be
# "colony4j-NNNN.mp4" where NNNN is a long UUID string. For now, only grayscale is supported.
