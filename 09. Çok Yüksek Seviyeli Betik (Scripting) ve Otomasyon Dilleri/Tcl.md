# Tcl

## Özet
Tcl (Tool Command Language - *Tıkıl* veya *Ti-Si-El* olarak okunur); 1988 yılında John Ousterhout tarafından icat edilen, evrendeki her şeyin (kodun bile) aslında saf bir **String (Metin)** olarak kabul edildiği, son derece küçük bir C-kütüphanesi olarak C programlarının kalbine "Komut/Arayüz desteği" katmak için doğmuş, ve **Tk** isimli Grafik Arayüz (GUI) eklentisiyle 90'lı yılların "Ekrana Pencere çizme" endüstrisini domine etmiş kült ve radikal bir betik dilidir.

## Nedir ve Ne İşe Yarar?
1980'lerde programcılar harika C uygulamaları yazıyordu (Örneğin bir veritabanı veya bir Çizim Aracı) lakin bu programların kullanıcının yazılarıyla etkileşime gireceği "Dinamik bir Terminal/Komut (Script)" sistemi yoktu. Herkes kendi C programına uyduruk, kötü if/else komut okuyucuları yazıyordu.

John Ousterhout, "Size çok hafif, sadece metinleri ve komutları okuyan bir (Engine/Motor) vereyim, bunu C uygulamanızın içine Gömün (Embed)" dedi. Böylece "Tcl" doğdu. O kadar hafif ve harikaydı ki, ağ (Networking) cihazlarına (Örn: Cisco yönlendiricileri) gömüldü. Ama Tcl'in dünyayı asıl fethi, çok gariptir ki kendisi değil "Kardeşi" yüzünden oldu. Tcl için geliştirilen **Tk (Tool Kit)** arayüz pakedi; Bir C/Python/Perl kullanıcısının ekrana bir "Button" ve "Sayfa" basmasının evrendeki EN KOLAY yolu olduğu için Tcl/Tk bir efsaneye dönüştü. (Bugün bile Python indirdiğinizde gelen *Tkinter* aslında Tcl/Tk'dır!).

**Ne İşe Yarar?**
* **Elektronik Tasarım Otomasyonu (EDA) Cihazları:** Dünyadaki Çip (CPU/FPGA) tasarım donanım uygulamalarının (Synopsys, Xilinx, Cadence) içinde çiplere komut veren (Script) %90 "Tek Dil" vardır: Tcl. Hızlı elektronik endüstrisinin betik anahtarıdır.
* **Ağ (Routers) Otomasyonu:** Cisco (IOS) cihazların içine gömülü gelen ve Router ayarlarını (expect kütüphanesi ile) otomatize eden yapı.

## Dilin Mantığı ve Kod Yapısı
Tcl, bir programlama dili gibi değil; bir **"Komut Dosyası (Command Shell)"** gibi davranır. Her şey Boşluklarla (Space) ayrılmış komutlardır. (Lisp gibidir ama parantezsizdir).
En korkunç (Radikal) felsefesi: *Tcl'de TİP (Type) yoktur. "15" sayısı, 15 değildir, "1", "5" harflerinden oluşan şaf bir Düz Yazıdır (String)*. Matematiksel işlem görene kadar Tcl dünyayı metin olarak görür (Everything is a string).

Köşeli parantezler `[ ]` içine yazılan komutu "Çalıştırıp Çıktısını Al (Command Substitution)", Dolar işareti `$` ise "Değişkenin İçiniOku" manasına gelir.

**Örnek İşleyiş (Sembolik Olarak):**
Python'da Mat: `x = 5 + 5; print(x)`
Tcl Tipi Cümleler: `set x [expr 5 + 5]; puts $x` (Kelime kurar gibi; x'i Ayarla, Neye? 'expr 5+5'in' çalışma Çıktısına! Onu da Puts(Bas)!).

### Örnek Bir Tcl Kodu: "Tk" İle (Python/Tkinter'ın Atası) Ekrana Pencere ve Buton Basmak
Donanımcıların devasa C/FPGA aletlerini ufak Tcl Scriptleriyle yönettiği (Puts, Set) kurgusuna; küçük, şirin bir Görsel Pencere (Button) basan klasik Tk Entegrasyonu:

```tcl
# Tcl dilinde Yorumlar '#' isareti ile yapiilir. 
# ANCAK DIKKAT: Yorumlar sadece ve sadece Satir Basindaysa Gecerlidir! Tcl gariptir.

# 1. DEĞİŞKEN (STRING) ATAMALARI
# set komutu ile 'kullanici' isminde Kutuya(Hafizaya) "Hardware Muhendisi" METNİNİ bas 
set kullanici "Hardware Muhendisi"

# 2. KOMUT ÇALIŞTIRMA VE HESAPLAMA (Köşeli Parantez ve expr)
# Matematik Yapmak icin 'expr (Evaluate Expression)' komutuna zorunlusunuzdur! Cünkü hersey Metindir:
set sonuc [expr 10 * 20]

# Puts: Ekrana (Console'a) String Bas, Çıktı ver!
puts "Merhaba, $kullanici! Sistem Testi Basladi. Hesap= $sonuc"


# 3. KOSULLAR (IF) VE KIVIRCIK PARANTEZLER (GECİKME)
# Tcl'de if'in yanindaki Kivrircik Parantez {} kod blogu degil; 
# C'deki/Lisp'teki gibi "Icindeki kodlari String Olarak DONDUR(Suspend), Ihtiyac aninda calistir" anlamindadir!
if {$sonuc > 100} {
    puts "Sonuc 100'den Buyuk. Gise Hatasiz."
}


# ===============================================================
# 4. TK (TOOL KIT) ILE MUCİZE TIKLANABILIR GRAFIK PENCERESİ!
# Python Tkinter'ina Ruhu veren o Saf Orijinal Kod:
# ===============================================================

# package require Tk (Eksi Tcl'lerde Gerekirdi, artik dahil)

# ".mesajam" -> Nokta ile baslayan Isimler Ana-Pencerenin(Root) altındaki Objelerdir!
# "label" (Etiket metin kutusu) olustur, Yazi Ekle:
label .mesaj -text "Çip Testi Tamamlanıyor $kullanici!" -foreground "blue"

# "button" (Tiklanabilir Buton) olustur, Ustunde Cikis yazsin:
# command -> Butona basilinca Hangi TCL Kodunu çalistirsin? "exit" kodu:
button .buton_cikis -text "Testi Bitir (Kapat)" -command { exit }

# PACK MİMARISI: Olusturulan widgetlari Pencerin icine Ust Uste Pakitle (Sigdir)
pack .mesaj .buton_cikis -padx 20 -pady 20

# Bu kod Calisince Ortada Masmavi yazili, altinda Buton olan 2x2 Cok islevli bir Uygulama aninda Acilir.
```

Tcl yorumlayıcısı (Interpreter) bu satırları tek tek kelime olarak çeker. "button" gördüğü an (Eğer C programıysanız) onu sizin C'de yarattığınız `draw_button()` API çağrısına bağlar ve yazılımı uçurur.

## Kimler Kullanır?
* Evrendeki bütün FPGA, ASIC ve İşlemci üreten **Elektronik (Hardware/Donanım) Mühendisleri!** Intel, AMD veya Aselsan mühendisleri mikroçipleri simüle edip derlerken arka plandaki tüm otomasyon (Örn Modelsim do dosyaları, Vivado xdc scriptleri) saf Tcl felsefesine dayanır. (Bu piyasa Asla Tcl'den vazgeçmez).
* Zamanında Cisco router (Cihaz yapılandırma / Expect kütüphanesi) uzmanları.
* Asla Web, Mobil veya Oyun yapmak için kullanılabilen bir dil değildir; donanım ile C dilini konuşturan tutkal (Glue Language) ve metin yığıcıdır.
