#!/usr/bin/env python
##
##  skk2cdb.py - convertion tool for SKK dictionary.
##  by Yusuke Shinyama
##  * public domain *
##
##  usage: skk2cdb.py [-f] outfile [infile ...]
##  options: -f: force overwriting.
##
##  example:
##    $ skk2cdb.py SKK-JISYO.L.cdb SKK-JISYO.L
##

# taken from pycdb.py - Python implementation of cdb 

import sys, os
from struct import pack, unpack
from array import array


# calc hash value with a given key
def cdbhash(s, n=0L):
  return reduce(lambda h,c: ((h*33) ^ ord(c)) & 0xffffffffL, s, n+5381L)

if pack('=i',1) == pack('>i',1):
  # big endian
  def decode(x):
    a = array('I', x)
    a.byteswap()
    return a
  def encode(a):
    a.byteswap()
    return a.tostring()
else:
  # little endian
  def decode(x):
    a = array('I', x)
    return a
  def encode(a):
    return a.tostring()


##  CDB
##

# cdbiter
def cdbiter(fp, eod):
  kloc = 2048
  while kloc < eod:
    fp.seek(kloc)
    (klen, vlen) = unpack('<II', fp.read(8))
    k = fp.read(klen)
    v = fp.read(vlen)
    kloc += 8+klen+vlen
    yield (k,v)
  fp.close()
  return


# CDBReader
class CDBReader:
  
  def __init__(self, cdbname, docache=1):
    self.name = cdbname
    self._fp = file(cdbname, 'rb')
    hash0 = decode(self._fp.read(2048))
    self._hash0 = [ (hash0[i], hash0[i+1]) for i in xrange(0, 512, 2) ]
    self._hash1 = [ None ] * 256
    (self._eod,_) = self._hash0[0]
    self._docache = docache
    self._cache = {}
    self._keyiter = None
    self._eachiter = None
    return

  def __getstate__(self):
    raise TypeError

  def __setstate__(self, dict):
    raise TypeError

  def __getitem__(self, k):
    k = str(k)
    if k in self._cache: return self._cache[k]
    h = cdbhash(k)
    h1 = h & 0xff
    (pos_bucket, ncells) = self._hash0[h1]
    if ncells == 0: raise KeyError(k)
    hs = self._hash1[h1]
    if hs == None:
      self._fp.seek(pos_bucket)
      hs = decode(self._fp.read(ncells * 8))
      self._hash1[h1] = hs
    i = ((h >> 8) % ncells) * 2
    n = ncells*2
    for _ in xrange(ncells):
      p1 = hs[i+1]
      if p1 == 0: raise KeyError(k)
      if hs[i] == h:
        self._fp.seek(p1)
        (klen, vlen) = unpack('<II', self._fp.read(8))
        k1 = self._fp.read(klen)
        if k1 == k:
          v1 = self._fp.read(vlen)
          if self._docache:
            self._cache[k] = v1
          return v1
      i = (i+2) % n
    raise KeyError(k)

  def get(self, k, failed=None):
    try:
      return self.__getitem__(k)
    except KeyError:
      return failed

  def has_key(self, k):
    try:
      self.__getitem__(k)
      return True
    except KeyError:
      return False

  def __contains__(self, k):
    return self.has_key(k)

  def firstkey(self):
    self._keyiter = None
    return self.nextkey()
  
  def nextkey(self):
    if not self._keyiter:
      self._keyiter = ( k for (k,v) in cdbiter(self._fp, self._eod) )
    try:
      return self._keyiter.next()
    except StopIteration:
      return None

  def each(self):
    if not self._eachiter:
      self._eachiter = cdbiter(self._fp, self._eod)
    try:
      return self._eachiter.next()
    except StopIteration:
      return None
  
  def iterkeys(self):
    return ( k for (k,v) in cdbiter(self._fp, self._eod) )
  def itervalues(self):
    return ( v for (k,v) in cdbiter(self._fp, self._eod) )
  def iteritems(self):
    return cdbiter(self._fp, self._eod)


# CDBMaker
class CDBMaker:

  def __init__(self, cdbname, tmpname):
    self.fn = cdbname
    self.fntmp = tmpname
    self.numentries = 0
    self._fp = file(tmpname, 'wb')
    self._pos = 2048                    # sizeof((h,p))*256
    self._bucket = [ array('I') for _ in xrange(256) ]
    return

  def __len__(self):
    return self.numentries

  def __getstate__(self):
    raise TypeError

  def __setstate__(self, dict):
    raise TypeError

  def add(self, k, v):
    (k, v) = (str(k), str(v))
    (klen, vlen) = (len(k), len(v))
    self._fp.seek(self._pos)
    self._fp.write(pack('<II', klen, vlen))
    self._fp.write(k)
    self._fp.write(v)
    h = cdbhash(k)
    b = self._bucket[h % 256]
    b.append(h)
    b.append(self._pos)
    # sizeof(keylen)+sizeof(datalen)+sizeof(key)+sizeof(data)
    self._pos += 8+klen+vlen
    self.numentries += 1
    return self
  
  def finish(self):
    self._fp.seek(self._pos)
    pos_hash = self._pos
    # write hashes
    for b1 in self._bucket:
      if not b1: continue
      blen = len(b1)
      a = array('I', [0]*blen*2)
      for j in xrange(0, blen, 2):
        (h,p) = (b1[j],b1[j+1])
        i = ((h >> 8) % blen)*2
        while a[i+1]:             # is cell[i] already occupied?
          i = (i+2) % len(a)
        a[i] = h
        a[i+1] = p
      self._fp.write(encode(a))
    # write header
    self._fp.seek(0)
    a = array('I')
    for b1 in self._bucket:
      a.append(pos_hash)
      a.append(len(b1))
      pos_hash += len(b1)*8
    self._fp.write(encode(a))
    # close
    self._fp.close()
    os.rename(self.fntmp, self.fn)
    return

  # txt2cdb
  def txt2cdb(self, lines):
    import re
    HEAD = re.compile(r'^\+(\d+),(\d+):')
    for line in lines:
      m = HEAD.match(line)
      if not m: break
      (klen, vlen) = (int(m.group(1)), int(m.group(2)))
      i = len(m.group(0))
      k = line[i:i+klen]
      i += klen
      if line[i:i+2] != '->': raise ValueError('invalid separator: %r' % line)
      i += 2
      v = line[i:i+vlen]
      self.add(k, v)
    return self


# cdbdump
def cdbdump(cdbname):
  fp = file(cdbname, 'rb')
  (eor,) = unpack('<I', fp.read(4))
  return cdbiter(fp, eor)


# cdbmerge
def cdbmerge(iters):
  q = []
  for it in iters:
    try:
      q.append((it.next(),it))
    except StopIteration:
      pass
  k0 = None
  vs = None
  while q:
    q.sort()
    ((k,v),it) = q.pop(0)
    if k0 != k:
      if vs: yield (k0,vs)
      vs = []
    vs.append(v)
    k0 = k
    try:
      q.append((it.next(),it))
    except StopIteration:
      continue
  if vs: yield (k0,vs)
  return


# aliases
cdbmake = CDBMaker
init = CDBReader


# main
def main(argv):
  import getopt
  import fileinput
  import os.path
  def usage():
    print 'usage: %s [-f] outfile [infile ...]' % argv[0]
    return 100
  try:
    (opts, args) = getopt.getopt(argv[1:], 'dfo:')
  except getopt.GetoptError:
    return usage()
  force = False
  for (k, v) in opts:
    if k == '-f': force = True
  if not args: return usage()
  outfile = args.pop(0)
  if not force and os.path.exists(outfile):
    print >>sys.stderr, 'file exists: %r' % outfile
    return 1
  #
  maker = CDBMaker(outfile, outfile+'.tmp')
  for line in fileinput.input(args):
    line = line.strip()
    if line.startswith(';'): continue
    try:
      i = line.index(' ')
    except ValueError:
      continue
    (k,v) = (line[:i], line[i+1:])
    maker.add(k, v)
  maker.finish()
  return

if __name__ == '__main__': sys.exit(main(sys.argv))
