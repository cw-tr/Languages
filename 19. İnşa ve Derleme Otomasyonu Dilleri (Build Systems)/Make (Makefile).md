# Make (Makefile)

## Özet
Make; 1976 yılında Stuart Feldman tarafından Bell Laboratuvarları'nda icat edilen ve o günden bu yana Unix/Linux dünyasının kalbinde yatan; yüzlerce, hatta binlerce C/C++ dosyasından oluşan devasa projelerin **"Nasıl, hangi sırayla ve sadece değişen dosyalarını güncelleyerek"** derleneceğini (Compile) organize eden efsanevi **İnşa Otomasyonu (Build Automation)** dilidir. Yazılım dünyasındaki bütün modern derleyici araçlarının (CMake, Maven, Gradle, npm scripts) Büyük Büyükbabasıdır.

## Nedir ve Ne İşe Yarar?
1970'lerde C dili ile 100 dosyalık bir program yazdıysanız, bu programı makine diline çevirmek için tek tek 100 kere `gcc dosya1.c -o dosya1.o` yazmanız gerekiyordu. Ya da hepsini aynı anda derlemek (compile) 20 dakika sürüyordu.
Diyelim ki programdaki kodun 1 harfini değiştirdiniz. Bütün o 100 dosyayı YENİDEN derlemek için 20 dakika daha beklemek intihardı!

Stuart Feldman Make'i icat edip dedi ki: **"Bir Taktik Tahtası (Makefile) yazalım. Make, Hangi dosyanın ne zaman Değiştirildiğinin (Zaman damgası - Timestamp) kaydını tutsun. Eğer 100 dosyadan SADECE 1'i değişmişse, Gidip sadece o 1 Tanesini derlesin, Sonra diğer 99'uyla eski hallerini Birlestiriverin (Link)!"** 
Bu vizyon, Derleme Süresini 20 dakikadan 2 saniyeye düşürerek Bilişim Tarihini hızlandırdı.

**Ne İşe Yarar?**
* **Depedency Graph (Bağımlılık Ağacı) Çözme:** `A.c` dosyasının derlenmesi için `A.h`ye ihtiyaç varsa, Make bu ağacı Hiyerarşik olarak kurar ve Hata yapmadan doğru sırayla derler.
* **C/C++ Derleme Standardı:** Bugün internetten açık kaynak bir Linux çekirdeği programı indirdiğinizde her zaman Kurulum 3 adımdır: `./configure`, `make`, `make install`. O "make" sihirli kelimesi, klasördeki `Makefile`'ı Okuyup Cihaza uygun C derlemesini Işık Hızında başlatmanın Evrensel Emridir.

## Dilin Mantığı ve Kod Yapısı
Makefile tamamen **KURALLAR (Rules)** üzerine inşa edilmiştir.

Bir Kuralın Anatomisi Şöyledir:
```makefile
hedef(target): bagimliliklar(prerequisites)
<TAB>    bu_hedefi_nasil_yaratacagimi_gosteren_komut (recipe)
```
**CRITIK UYARI (Tarihin En Büyük Laneti):** Makefile içindeki O `<TAB>` yazan yer (Komutun girintisi) KESİNLİKLE Klavyedeki "TAB ↹" tuşu ile bırakılmalıdır! Eğer oraya klavyeyle 4 Tane "Boşluk (Space)" Atarsanız Makefile BOZULUR ve Çöker! Stuart Feldman bu sekilde dizayn etmiş ve 1976'dan beri milyarlarca yazılımcı bu "Tab/Space Hatası" yüzünden Ekran başında Saç Baş yolmuştur.

### Örnek Bir Makefile Kodu: Bir C Programını Saniyede Derlemek
2 Tane C Kodu dosyamız var (`main.c` ve `matematik.c`). Bunlardan Bir "Uygulama.exe" Cıkarma reçetesi Şöyledir:

```makefile
# BU BIR MAKE KODUDUR (Dosya Adi Sadece: "Makefile" Olmalidir Uzantisi yoktur)

# 1. DEGISKENLER (Ortam Degiskenleri)
CC = gcc                    # Kullanilacak Derleyici (C Compiler = GCC)
CFLAGS = -Wall -g           # Derleyiciye Ayarlar (Tum uyarilari ac, HataAyiklama modu -g Cek)
HEDEF_PROGRAM = uyuglamamiz # Cikacak Olan Programin(Exe) Adi

# 2. ANA HEDEF (KOK KURAL): 'all' (Terminale sadece 'make' yazdigimizda Oynayaxak kural)
# Uygulamamiz programinin cikarilabilmsi iicin ÖNCE (main.o ve matematik.o) dosyalarina ihtiytaci vardir!
all: $(HEDEF_PROGRAM)

# 3. BAGLANTI (LINKING) ASSMASI: Nasil Olusacak?
# hedef: bagimlilik1 bagimlilik2 
$(HEDEF_PROGRAM): main.o matematik.o
	# TAB TUSU ILE BASLANAN KOMUT SATIRI: Gcc yi cagir, O iki ".o" (Obje) dosyasini alip Biirlestir
	$(CC) $(CFLAGS) -o $(HEDEF_PROGRAM) main.o matematik.o

# 4. OBJELERI TEK TEK DERLEME KURALLARI
main.o: main.c
	# Sadece main.c'yi derleyip Ara-Format olan "main.o" ya cevr! (C isretiii compile etmketiR)
	$(CC) $(CFLAGS) -c main.c

matematik.o: matematik.c fonksiyonlar.h
	# Eger matematik.c VEYA icindeki Kutuphane(fonksinlar.h) son 1 saattse Degistirilmisse burayi TETIkLE:
	$(CC) $(CFLAGS) -c matematik.c

# 5. TEMIZLIK KURALI (Ortaligi Toparlama)
clean:
	# Terminale 'make clean' yazildiginda Cıkan Bütün Çöpleri(.o dosyalari ve exe yi) Sil:
	rm -f *.o $(HEDEF_PROGRAM)

```

## Kimler Kullanır?
* Evrendeki istisnasız BÜTÜN **C ve C++ Geliştiricileri, Gömülü Sistem(IoT) Mühendisleri, Linux Çekirdek(Kernel) Geliştiricileri**.
* Makefile o kadar sağlamdır ki, C Dışındaki dillerde bile (Örn Python veya Go projelerinde) "Hızlı Otomasyon Scripti" (Düğme) olarak kullanılır. Adam `make docker-baslat` içine upuzun 15 satırlık Docker komutu gizler ve Pratikçe takımı yönetir.
* Tarihi değeri paha biçilemez ve hala her gün Aktif çalışan ender Yarı-Asırlık dillerden biridir. Mükemmeldir lakin o Tab/Boşluk sıkıntısıyla C++ da kan kusturtur ve Yerini yavaşyavaş (CMake)'e Bırakmaya başlamıştır.
