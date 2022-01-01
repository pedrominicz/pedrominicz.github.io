const stage = document.getElementById('stage');

class Model {
  static placed = [];

  constructor(id, model, scale, position, rotation) {
    const el = document.createElement('a-entity');

    this.id = id;
    this.el = el;
    this.rotation = rotation;

    el.setAttribute('gltf-model', `#${model}`);

    const [x, y, z] = position;

    el.setAttribute('position', `${x} ${y + 0.5} ${z}`);

    if(scale) {
      el.setAttribute('scale', `${scale} ${scale} ${scale}`);
    }

    if(rotation) {
      const [x, y, z] = rotation;

      el.setAttribute('rotation', `${x} ${y} ${z}`);
    }

    el.setAttribute('animation', `
      property: position;
      from: ${x} ${y + 0.5} ${z};
      to: ${x} ${y} ${z};
      dur: 700;
    `);

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
    this.el.setAttribute('rotation', '0 0 0');
  }

  setOnClickHandler() {
    this.el.addEventListener('click', () => this.onClick());
  }

  place() {
    Model.placed.push(this.id);

    if(this.rotation) {
      const [x, y, z] = this.rotation;
      this.el.setAttribute('rotation', `${x} ${y} ${z}`);
    }

    this.el.emit('place');
  }

  remove(index) {
    Model.placed.splice(index, 1);

    const [x, y, z] = this.shelfPosition;

    this.el.setAttribute('position', `${x} ${y} ${z}`);
    this.el.setAttribute('rotation', '0 0 0');

    this.el.removeAttribute('animation');
  }

  onClick() {
    const index = Model.placed.indexOf(this.id);

    if(index === -1 && this.id > Math.max(...Model.placed)) {
      this.place();
    } else {
      if(index !== -1) {
        this.remove(index);
      } else {
        this.el.emit('spin');
      }
    }

    if(Model.placed.length === models.length) {
      setTimeout(() => stage.emit('win'), 1000);
    }
  }
}

const start = async () => {
  for(const [index, model] of models.entries()) {
    models[index] = new Model(index, model.m, model.s, model.p, model.r);

    console.log(stage.appendChild(models[index].el));

    await new Promise(resolve => setTimeout(resolve, 1200));
  }

  // Shuffle models.
  models.sort(() => Math.random() - 0.5);

  if(Math.random() < 0.5) {
    models.reverse();
  }

  for(const [index, model] of models.entries()) {
    const x = models.length - 2*index - 1;

    model.setShelfPosition([x, 0, -3]);

    model.setOnClickHandler();
  }
}

start();
