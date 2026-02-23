# Bash (Bourne Again SHell)

## Özet
Bash; 1989'da Brian Fox tarafından (GNU projesi için) işletim sisteminin tam kalbine, komut satırı (Command-Line) ara yüzüne yazılmış; dünyanın dünyasını yöneten tüm Linux, macOS ve Unix işletim sistemlerinin vazgeçilmez kök (Root) kabuk ve otomasyon betik/scripting dilidir.

## Nedir ve Ne İşe Yarar?
Eğer bir işletim sisteminde Fare (Mouse) ile klasör açıyor, içindeki metin dosyasının sonuna bir kelime ekliyor ve sonra bu dosyayı internette bir klasöre yüklüyorsanız (Sürükle-Bırak/Arayüz kullanarak), muhtemelen bunu yapmak 2-3 dakikanızı almıştır.

Eğer elinizde 50 Milyon dosya varsa bunu Fare ile yapamazsınız. Linux tabanlı bir Kernel'e (Çekirdeğe), "Bu klasördeki içinde 'A' harfi geçen şu milyonlarca dosyayı önce bul, ZIP'le, ismini değiştir, sonra sunucuya SSH'la at" komutunu Saniyeler içinde, dümdüz kelimeler yazarak verdiren programın kendisidir BASH.

**Ne İşe Yarar?**
* **Sistem Yöneticileri (Sysadmin / DevOps):** Bütün AWS Veri Merkezleri, Bulut İşletim Sistemleri, Milyonlarca Web Sunucusu günde bir kez yedek almak, içindeki yığılmış çöpleri silmek gibi ağır sunucu otomasyonları (Cron Jobs / Shell Scripts) için Bash'a muhtaçtır.
* **Continuous Integration (CI/CD):** Yazılımcıların GitHub'a/Sunucuya proje yolladıklarında arka planda tetiklenen "Docker indir, Projeyi derle, Testleri çalıştır ve Canlı Siteyi Başlat" emir zinciri (Pipelines) tamamen Bash Script `.sh` dosyaları aracılığıyla ardışık çalışır. C diliyle falan Sunucu yönetilmez.

## Dilin Mantığı ve Kod Yapısı
Bash aslında geleneksel bir kodlama/programlama dilinden çok, İşletim Sisteminin komutlarını (örneğin "ls" listeleme, "cat" okuma, "grep" arama komutları) ucuca (boru hattı ile) bağlayan süper bir "Komut Yöneticisidir".

Tıpkı boru döşer gibi `|` (Pipe) operatörü kullanılır. "Milyon SATIR oku -> BORU(|) -> İÇİNDEN Hata KELİMESİNİ ARA (grep) -> BORU(|) -> BUNLARI txt'ye YAZ!". Değişken yaratıldığında boşluk kullanılamaz (`ISIM="Ahmet"`), ancak okurken mutlaka Dolar işareti koyulur (`$ISIM`). 

En dikkat çeken (Kişilerin öğrenirken en zorlandığı) yer ise, if-else bloklarını kapatma kelimelerinin tamamen Ters yazılmasıdır. Bir IF başlarsa `fi` ile biter; bir CASE başlarsa `esac` ile biter. 

**Örnek İşleyiş (Sembolik Olarak):**
Python'da olsaydınız "İşletim Sisteminin Harddisk kütüphanesini import et, Klasöre gir, Dosyaları döngüye al, Dosya formatını oku... " en az 20 satır harcardınız. Bash, aslında İşletim Sisteminin bizzat ta kendisi olduğu için `mv *.jpg /yedek` yazarsınız ve 10 bin resim saniyesinde taşınır biter!

### Örnek Bir Bash Kodu: Günlük Otomasyon (Backup / Yedekleme Scripti)
İşletim sistemine Python veya C kütüphaneleriyle debelenmeden, DOĞRUDAN saf Terminal (Terminal/Konsol) komutları ile Hükmetmek:

```bash
#!/bin/bash
# İlk satir MUCİZESİ (Shebang): 
# İşletim sistemine, "Bu dosyayi (betigi) buldugun an altindakileri BASH motoruna gonder calistir" fermanidir.

# Değişken Atama: (Eşittir isaretinin etrafinda BOSHUK OLAMAZ, yoksa sitem "Hedef" diye komut arar cöker)
HEDEF_KLASOR="/var/log"
YEDEK_DIZINI="/home/user/log_yedekleri"

# TARIH degiskenini (Dinamik), Sstemin arka komutu olan $(date) emrinden alıp Yil-Ay-Gun formatinda yapistiralim:
TARIH=$(date +%Y-%m-%d)
YEDEK_DOSYASI="yedek_$TARIH.tar.gz"

# 1. ADIM: Ekrana (Siyah Konsol Ciktisi) bilgi firlat
echo "=== Yedekleme Basliyor: $TARIH ==="

# 2. ADIM: İf Else Koşulu (Gramer C-Dillerinden Cok Farklidir!)
# "-d" flag'i (bayragi): "Boyle bir DIZIN (klasor) GERÇEKTEN var mi?" diye kontrol eder
if [ -d "$YEDEK_DIZINI" ]; then
    echo "Yedekleme klasoru zaten var, sorun yok."
else
    echo "Klasor yok, Otomatik Yaratiliyor..."
    # Asil is: mkdir (Make Directory - Klasor Ac) Emrini arka planda İsletim Sistemine Yolla!
    mkdir -p "$YEDEK_DIZINI"
fi 
# DİKKAT: "if" kosulunu kapatmak icin ters yazilisi olan "fi" kullanilir. BASH estetigidir.

# 3. ADIM: DEVASA SISTEM ALGORITMASI (Tar Komut Satiri) 
# Hicbir harici kütüphane aramadan isini yap ve Ziple
# Anlami: HEDEF klasordeki HER SEYI (-c create, -v gostererek, -z zipla, -f dosyaya) -> YEDEK_DOSYASI'na at!
tar -czf "$YEDEK_DIZINI/$YEDEK_DOSYASI" "$HEDEF_KLASOR"

# 4. ADIM: Boru (/Pipe |) ile Hata Ayiklama Gucu:
# LS(Listele) emrini at. Ciktilari Borudan(|||) diger isciye aktar. SADECE BUGÜN TARİHLİ olana GREP at (Yakala)
echo "Yedek Basariyla Tamamlandi. Iste Olusan Dosya Detaylari:"
ls -lh "$YEDEK_DIZINI" | grep "$TARIH"

# Komutu Kapat.
exit 0
```
Bu `.sh` eklentili o kısacık dosya sayesinde bir şirketin Milyarlarca Gigabayt veri sunucusu gece yarısı uyurken bir satır komutla kendini kilitler, yedekler ve sabah tekrar hizmete açar.

## Kimler Kullanır?
* Hemen hemen yazılım yayan Milyonlarca **Backend ve DevOps (Bulut / Linux Server)** Mühendisi. Sunucu kiraladığınız hiçbir bilgisayar Cihaz ekranıyla/mouse ile gelmez. (AWS, Google Cloud vs Simsiyah bir Komut satırıdır). Sadece SSH ile BASH kodlayarak içerik yazabilirsiniz.
* Ethical Hackerlar, Ağ yöneticileri (Routers ve Firewalls).
* Bilim İnsanları ve Araştırmacılar, 50 bin Excel / PDF dökümanının ismini teker teker Rename "1.pdf, 2.pdf" diye çevirmek yerine `for f in *.pdf; do mv "$f" "Yeni_$f"; done` BASH satırını yazarak saatlerce vakit tasarrufu yaparlar.
