# WeasyPrint on Windows

WeasyPrint is a powerful tool for converting HTML/CSS documents into PDF.  
On Windows, it requires additional libraries (GTK, Cairo, Pango, GDK-Pixbuf, GLib) that are not bundled with Python.  
This guide explains how to set up WeasyPrint correctly using **MSYS2** (via Scoop or standalone).

---

## 📦 Prerequisites

- **Python 3.9+** (installed via [Python.org](https://www.python.org/downloads/) or Scoop)
- **pip** (comes with Python)
- **MSYS2** (installed via [Scoop](https://scoop.sh/) or [MSYS2.org](https://www.msys2.org/))

---

## 🔧 Step 1: Install MSYS2 Dependencies

Open the **MSYS2 MinGW64 shell** and install required packages:

```bash
pacman -S mingw-w64-x86_64-glib2 \
          mingw-w64-x86_64-pango \
          mingw-w64-x86_64-cairo \
          mingw-w64-x86_64-gdk-pixbuf2
```

These provide the DLLs (`libgobject-2.0-0.dll`, `libglib-2.0-0.dll`, `libpango-1.0-0.dll`, etc.) that WeasyPrint depends on.

---

## 🔧 Step 2: Add MSYS2 to PATH

WeasyPrint runs under Windows Python, so it must find the DLLs.  
Add the following folder to your PATH:

```
C:\Users\<YourUser>\scoop\apps\msys2\current\mingw64\bin
```

> Replace `<YourUser>` with your Windows username.

### Permanent PATH update (PowerShell)

```powershell
[System.Environment]::SetEnvironmentVariable(
  "Path",
  $env:Path + ";C:\Users\<YourUser>\scoop\apps\msys2\current\mingw64\bin",
  [System.EnvironmentVariableTarget]::User
)
```

Restart your terminal or IDE after updating PATH.

---

## 🔧 Step 3: Install WeasyPrint

Install via pip:

```bash
pip install weasyprint
```

---

## ✅ Step 4: Verify Installation

Check that Python can load the libraries:

```python
import ctypes
ctypes.CDLL("libgobject-2.0-0.dll")
```

If no error appears, WeasyPrint is ready.

Test WeasyPrint:

```bash
python -m weasyprint https://weasyprint.org weasyprint.pdf
```

You should get a `weasyprint.pdf` file.

---

## 🛠 Troubleshooting

- **OSError: cannot load library 'libgobject-2.0-0'**
  - Ensure `mingw64\bin` is in PATH, not `usr\bin`.
  - Verify DLL exists:  
    ```powershell
    Get-ChildItem "C:\Users\<YourUser>\scoop\apps\msys2\current\mingw64\bin" | findstr gobject
    ```

- **Fonts not rendering correctly**
  - Install additional font packages in MSYS2:  
    ```bash
    pacman -S mingw-w64-x86_64-freetype mingw-w64-x86_64-fontconfig
    ```

- **CSS not applied**
  - WeasyPrint supports most CSS2.1 and parts of CSS3. Check WeasyPrint CSS support [(doc.courtbouillon.org in Bing)](https://www.bing.com/search?q="https%3A%2F%2Fdoc.courtbouillon.org%2Fweasyprint%2Fstable%2Ffeatures.html").

---

## 📚 References

- [WeasyPrint Documentation](https://doc.courtbouillon.org/weasyprint/stable/)
- [MSYS2 Packages](https://packages.msys2.org/)
- [Scoop](https://scoop.sh/)

---

## 🎯 Summary

1. Install MSYS2 via Scoop.  
2. Use `pacman` to install GTK-related libraries.  
3. Add `mingw64\bin` to PATH.  
4. Install WeasyPrint via pip.  
5. Verify with a test PDF.

With this setup, WeasyPrint runs smoothly on Windows.