
import os
import time
import json
from PIL import Image
from PIL import ImageDraw
import imageio


class Entity:
    def __init__(self, entity_id, location):
        self._id = entity_id
        self.x, self.y = location
        self.radius = 10

    def draw(self, image_draw, center):
        cx, cy = center
        x, y, r = self.x, self.y, self.radius
        bounds = cx + x - r, cy + y - r, cx + x + r, cy + y + r
        image_draw.ellipse(bounds)


def convert_object_to_entity(o):
    print(o)
    return Entity(o['id'], o['location'])


def convert_line_to_entities(line):
    return map(convert_object_to_entity, json.loads(line))


def main():
    input_file = './output/simulation'
    output_path = './output/images/%s/' % int(time.time())
    file_name = 'image'
    image_size = 1000, 1000
    image_center = image_size[0]/2, image_size[1]/2

    filenames = []

    print('Creating images')
    with open(input_file) as f:
        os.makedirs(output_path)
        num = 0
        for line in f:
            entities = convert_line_to_entities(line)
            image = Image.new('RGB', image_size, 'black')
            image_draw = ImageDraw.Draw(image)

            for entity in entities:
                entity.draw(image_draw, image_center)

            filename = '%s%s-%s.png' % (output_path, file_name, num)
            image.save(filename)
            filenames.append(filename)
            num += 1

            del image
            del image_draw

    print('Creating GIF')
    with imageio.get_writer('%ssimulation.gif' % output_path, mode='I') as writer:
        for filename in filenames:
            image = imageio.imread(filename)
            writer.append_data(image)

    print('Removing image files')
    for filename in filenames:
        os.remove(filename)





if __name__ == '__main__':
    main()
