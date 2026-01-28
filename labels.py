class Labels:
    def __init__(self):
        self._n_labels = 0

    def new_label(self, name):
        name = f'_{name}_{self._n_labels}'
        self._n_labels += 1
        return name
