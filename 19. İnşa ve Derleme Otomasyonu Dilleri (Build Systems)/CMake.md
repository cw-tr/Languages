# CMake

## Özet
CMake (Cross-Platform Make); 2000 yılında Kitware firması tarafından yaratılan, doğrudan yazılımı "derleyen(compile)" bir motor olmak yerine; Projenin işletim sistemine (Windows/Linux/Mac) bakarak Doğru İşletim sistemi C++ Ayarlarını Çıkaran ve o sistemin kendi derleyicisine (Windows ise Visual Studio File, Linux ise Makefile) uygun **"Ara Sürüm İnşa Dosyaları Üreten" Bir Meta-Derleme (Build-System Generator) Dilidir**. 

## Nedir ve Ne İşe Yarar?
Makefile (Make) harikaydı, Lakin Büyük bir Kabusu vardı: **İşletim Sistemleri Ayrıcalığı (Cross-Platform Kabusu)**.
Eğer siz Linux için Kusursuz bir `Makefile` yazarsanız, o kod Linux/gcc'de çalışırdı. Ancak projeyi arkadaşınıza Windows Laptobuna attığınızda `Makefile` Windows'un (Visual Studio'sunun / MSVC) komutlarını Anlamadığı İçin Proje anında Patlardı! Ekipler 1 Proje için 3 Ayrı Kural dosyası yazmak (Amelelilk) zorudna Kalırlardı.

CMake Dedi ki: Üzülmeyin! Siz Bana **"Bu Projenin İsmi X, İçinde şu C++ dosyaları Var"** Diye Kendi dilimde(CMakeLists.txt) Bir komut girin. Benim Motorum Bilgisayara Bakacak.. Bilgisayar Windows mu? O Zaman Visual Studio (`.sln`) Çıktısı Doğuracağım! Cihaz Linux mu? O Zaman sana O Cihaza özel Mükkeml bir `Makefile` Üreteceğim!

**Ne İşe Yarar?**
* **Çapraz Platform (Windows/Linux/Mac) C++ Projeleri:** Bugün OpenCV, Blender, KDE Desktop gibi milyarlarca Satırlık C++ projelerini internetten indirdiğinizde, İçerisinde `CMakeLists.txt` görürsünüz. Bu sayede her işletim sistemi bu programı Sorunsuz kendi Ana(Native) derleyicisine göre Derler.
* **Kütüphane Yönetimi:** C++ dünyasında "Yücelerin Paketi İndirici (NPM'i)" yoktur. Kütüphaneleri Projeye Linklemek (Ahmetin DLL'ni Meryemin EXE'sine baglamak) Cehennemdir. CMake Bu "Linking" (Birlestirme) Aşamalarını `target_link_libraries` büyüsüyle otomatikleştirerek Dünyevi bir Acıyı Sondurur.

## Dilin Mantığı ve Kod Yapısı
Tamamen Kendi Özel Sintaksı ve Argüman yapısı (Fonksiyon gibi Parantezli komutlar) vardır. C'ye de C++'a de benzemez, Tuhaf, İğrenç Derecede Ucube(Kirli)  ama Bir O kadar Görevini Yapmada Kusursuz bir dildir. Proje kök klasörüne **`CMakeLists.txt`** adıyla atılır.

Fonksiyonları Şöyledir: `KOMUT_ADI( ARGÜMAN1 ARGÜMAN2 )` ve tamamen büyük/küçük harf bağımsız(ama genelde Büyük harf tercih edilir) eziyetidir.

### Örnek Bir CMake Kodu: 1 Saniyede Çapraz Platform C++ Projesi Ayaklandırmak
İçinde `main.cpp` ve `matematik.cpp` Olan Ufak C++ projenizi "Evrendeki Tüm Bilgisayarlarda(Linux/Win) çalışabilir Derlemeye" sokan O Kirli kod:

```cmake
# BU BIR CMAKE KODUDUR (Dosya Adi: CMakeLists.txt Olmek Zorunda)

# 1. MINIMUM CMAKE VERSIYONU (Kural Seti, Geriye Uyumluluk Icin Sarkatir)
cmake_minimum_required(VERSION 3.10)

# 2. PROJENIN ADI VE KULLANACAGIMIZ DIL
# Projenin Adi: HesapMakinesi ve DİLİ C++ Kodu(CXX)!
project(HesapMakinesi VERSION 1.0 LANGUAGES CXX)

# 3. C++ STANDARDIYONUNUN BELIRLENMESI
# Bizim Kodlarimizda modern C++17 Özellikleri var (Orn: std::optional), Gidip Windows'da C++98 ile derlemeye kalkmasin Diye Kilit vuruyorz:
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# 4. ÇIKTI PROGRAMI(EXE) YARAT (EXECUTABLE)
# Hangi C++ Dosylari pisecek ve Nerde Bitecek (Linking Adimi):
# Burada "HesapProgramim" adiinda bir EXE uret... Ve İcine "main.cpp" ile "matematik.cpp" Kodunu sıkıstır diyoruz.
add_executable(HesapProgramim 
    main.cpp 
    matematik.cpp
)

# 5. KUTUPHANE/HEADER(Başlık) EKLEMESI
# C++ daki #include "math.h" yi ararken Nerelere bakacagini Eklioyruzz:
target_include_directories(HesapProgramim PUBLIC "${PROJECT_SOURCE_DIR}/includes")

# Eger Disaridan Bir DLL / .SO Cekilecekse (Orbegn OPENGL Kutuplanrsi) Onlari Birlestir(Linkle):
# target_link_libraries(HesapProgramim PUBLIC OpenGL::GL)
```

Yazılımcı klasördeyken Konsola `cmake .` yazdığı saniye:
- Windows'taysa KLasörün içine Devasa bir Visual Studio solution (`.sln`) doğar! Cift tıklayıp F5'e basıp direk oynar.
- Lİnux'daysa Klasörün İlkine kusursuz bir `Makefile` doğrar. Konsola `make` basdığı an Exe Si Hazırlanır. **İşte Bu C++'in Kutsal Kasesidir!** Evrenin en zor dilini Evrensel uyumluluğa taşır.

## Kimler Kullanır?
* Evrendeki Neredeyse Tüm (Gelişmiş) **C ve C++ Geliştiricileri**. O kadar Endistüri standartıdır ki, Android NDK'dan Tutun, Oyun Motorlarının (Unreal/O3DE) Altyapılarına Kadar Cmakelistsiz bir Modern Proje Tahayül bile edilemez.
* Başta iğrenç sintaksıyla Çok Nefret edilse de, Şu an Visual Studio ve CLion gibi Popüler IDE'ler `CMakeLists.txt` Dosyasını gördüğü anda Arkaplanda Sessizce Çözümler ve Yazılımcıya "Play" Düğmesi Çıkartarak Mükemmel Geliştirci deneyim sunarlar (Varlığını Hissetmeden Kullanırsısınız).
