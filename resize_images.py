import glob
import PIL
from PIL import Image


def resize(fname, resized_fname, wanted_width=500):
    img = Image.open(fname)
    wpercent = (wanted_width / float(img.size[0]))
    hsize = int((float(img.size[1]) * float(wpercent)))
    img = img.resize((wanted_width, hsize), PIL.Image.ANTIALIAS)
    img.save(resized_fname)


if __name__ == "__main__":
    for fname in glob.glob('img/car/*/*.jpg') + glob.glob('img/car/*.jpg'):
        if fname.split('.')[0].split('_')[-1] != 'small':
            msg = "messing stuff up with bad '.' split for fname {}".format(fname)
            assert len(fname.split('.')) == 2, msg
            small_fname = fname.split('.')[0] + '_small.' + fname.split('.')[1]

            print "Resizing {}".format(fname)
            resize(fname, small_fname)

