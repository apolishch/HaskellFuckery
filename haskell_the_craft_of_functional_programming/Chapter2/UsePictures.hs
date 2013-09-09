module UsePictures where
import Pictures

rotateHorse, whiteHorse, singleCheckersSquare, checkersBoard :: Picture
checkersSquare :: Picture -> Picture -> Picture
rotateHorse = rotate horse

whiteHorse = horse

checkersSquare one two = above (beside one two) (beside two one)

singleCheckersSquare = checkersSquare white black
checkersBoard = checkersSquare (checkersSquare singleCheckersSquare singleCheckersSquare) (checkersSquare singleCheckersSquare singleCheckersSquare)

horseVariantOne = checkersSquare whiteHorse (invertColour whiteHorse)
horseVariantTwo = above (beside whiteHorse (invertColour whiteHorse)) (beside (flipV (invertColour whiteHorse)) (flipV whiteHorse))
horseVariantThree = above (beside whiteHorse (invertColour whiteHorse)) (beside (flipH (flipV (invertColour whiteHorse))) (flipH (flipV whiteHorse)))
horseVariantFour = above (beside whiteHorse (invertColour whiteHorse)) (beside (flipH (invertColour whiteHorse)) (flipH whiteHorse))
