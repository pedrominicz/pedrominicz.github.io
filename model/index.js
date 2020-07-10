const scene = document.querySelector('a-scene');

class Model {
  static placed = [];

  constructor(id, model, scale, position, rotation) {
    const el = document.createElement('a-entity');

    el.setAttribute('gltf-model', `#${model}`);

    if(scale) {
      el.setAttribute('scale', `${scale} ${scale} ${scale}`);
    }

    this.id = id;
    this.el = el;
    this.rotation = rotation;

    el.addEventListener('click', () => this.onClick());

    const [x, y, z] = position;

    el.setAttribute('animation__place', `
      property: position;
      from: ${x} ${y + 0.5} ${z};
      to: ${x} ${y} ${z};
      dur: 700;
      startEvents: place;
    `);

    el.setAttribute('animation__spin', `
      property: rotation;
      from: 0 0 0;
      to: 0 360 0;
      dur: 500;
      startEvents: spin;
    `);
  }

  setShelfPosition(position) {
    const [x, y, z] = position;
    this.shelfPosition = position;

    this.el.setAttribute('position', `${x} ${y} ${z}`);
  }

  onClick() {
    const placed = Model.placed;
    const index = placed.indexOf(this.id);
    const el = this.el;
    const max = Math.max(...placed);

    if(index === -1 && this.id > max) {
      placed.push(this.id);

      if(this.rotation) {
        const [x, y, z] = this.rotation;
        el.setAttribute('rotation', `${x} ${y} ${z}`);
      }

      el.emit('place');
    } else {
      if(index !== -1) {
        placed.splice(index, 1);

        const [x, y, z] = this.shelfPosition;

        el.setAttribute('position', `${x} ${y} ${z}`);
        el.setAttribute('rotation', '0 0 0');

        el.removeAttribute('animation');
      } else {
        console.log('emit');
        el.emit('spin');
      }
    }
  }
}

const pieces = [];

for(const [index, model] of stage.entries()) {
  pieces.push(new Model(index, model.m, model.s, model.p, model.r));
}

// Shuffle pieces.
pieces.sort(() => Math.random() - 0.5);

for(const [index, model] of pieces.entries()) {
  const x = pieces.length - 2*index - 1;
  model.setShelfPosition([x, 0, -3]);

  scene.appendChild(model.el);
}
