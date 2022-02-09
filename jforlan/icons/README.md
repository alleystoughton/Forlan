JForlan's Icon
======================================================================

* `JForlan.xcf`: the GIMP XCF image for JForlan's icon; changes
  should be made to this file, from which the others are generated
  using GIMP and `iconutil`

* `JForlan-512x512.png`: the 512x512 PNG version -- seed for
  `JForlan.icns` (made using GIMP)

* `JForlan-128x128.png`: the 128x128 PNG version -- seed for
  `JForlan.icns` (made using GIMP)

* `JForlan.icns`: the Mac OS X icons file, constructed from
  JForlan-512x512.png and JForlan-128x128.png using iconutil

  First create a directory `JForlan.iconset` with elements
  `icon_128x128.png` and `icon_512x512.png` correponding to the two seeds.

  Then run

  ```
    iconutil -c iconutil JForlan.iconset
  ```
  to produce `JForlan.icns`.

* `JForlan-48x48.png` (made using GIMP): the 48x48 PNG version -- for
  GNOME/KDE

* `JForlan-48x48.ico` (made using GIMP): the 48x48 ICO version -- for
  Windows
