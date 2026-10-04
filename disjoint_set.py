class DisjointSet:
    def __init__(self):
        self._map = {}

    def union(self, x, y):
        self._map[x] = y

    def find(self, x):
        if x in self._map:
            result = self._map[x]
            return self.find(result)
        return x

    def is_empty(self):
        return not self._map
