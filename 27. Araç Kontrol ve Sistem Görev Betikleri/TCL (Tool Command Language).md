# TCL (Tool Command Language)

## Özet
TCL (Tickle diye okunur); 1988 yılında John Ousterhout tarafından icat edilen, "Her şey ama İSTİSNASIZ her şey bir Karakter Dizisidir (String)" felsefesiyle yaratılmış, Özellikle Elektronik Tasarım Otomasyonları (EDA - Çip üretim yazılımları) ile Cisco Router/Ağ cihazlarının iç yapılarında komut tetikleyici (Gömülü Scripting) olarak kullanılan, çok tuhaf ve eşi benzeri olmayan bir **Evrensel Komut Dizisi (Script)** dilidir.

## Nedir ve Ne İşe Yarar?
1980'lerde eğer siz "AutoCAD" ya da bir "Çip (CPU) Çizim Programı" yazdıysanız; Müşterilerinize programın içinde kullanabilmeleri için bir Makro/Betik (Script) dili sunmak zorundaydınız.
O dönemde Python popüler değildi. Mühendisler her C programı için uyduruk, yarım yamalak betik dilleri yazıp duruyordu.

John Ousterhout dediki: **Öyle bir Dil(TCL) yapacağım ki; Gidip bunu Saniyeler İçinde kendi C/C++ programınızın İÇİNE GÖMEBİLECEKSİNİZ! (Embeddable). Üstelik Bu dilin Matematikten(Integer) falan haberi olmayacak. Koddaki her şey Düz Yazı(String) olacak!**

**Ne İşe Yarar?**
* **Elektronik Çip Tasarımı (EDA):** Bugün Synopsys, Cadence veya Mentor Graphics gibi milyar dolarlık İşlemci(Çip/Devre) çizim programlarında, donanım mühendislerinin "Bu 10.000 adet transistörü Rastgele Şuraya Kopyala/Test Et" dediği makro komutları İstisnasız TCL diliyle yazılır.
* **Ağ (Network) Cihazları:** Cisco marka Switch veya IP Telefonlarına uzakttan Makro/Ayar yollarken, Cihazın içindeki kabuk(Shell) `tcl` komutlarını okur. `expect` isimli efsanevi eklentisi sayesinde SSH veya Telnet üzerinden Karşı Cihazı "Bekleyip-Yanıt Mekenizmasıyla" Tıpkı Bir İnsanmış Gibi Otonom Hekler.

## Dilin Mantığı ve Kod Yapısı
Tamamen **Komut (Command)** ve **Argüman (Strings)** odaklıdır.
Eğer siz TCL'de `set a 5` yazarsanız, TCL `5` sayısını Rakam(Integer) olarak algılamaz. Onu `5` karaktari (String) olarak algılar. Matematik Yapmak isterseniiz `expr` (Expression-MatematikYap) komutuu Çagirmank Zorundasinizi!

### Örnek Bir TCL Kodu: Otomasyon ve Bekleme Makrosu (Networkçülerin Gözdesi)
Basit Bir Değişken Tanımlamsi ve Matematik Çilelesi (Her şeyin String olmasindan Kaynaklanan Tuhaflık):

```tcl
# BU BIR TCL KODUDUR (Yorum Satiri Diez'dir)

# 1. DEGISKEN ATAMA (DİKKAT: "=" eşittir semblü YoKtuUR! Komut olarak yazlir!)
set sayi1 10
set sayi2 20

# 2. DEGİSKEKNI CAGGIRMA (Dolar İsaretilyedir!)
# Ekranda 1020 cikar! Cunkı Bunlar Metin(StrinG!dir Yanyana Yapistiririir!!
puts "$sayi1$sayi2" 

# 3. MATEMATİYE CEVIIRMME(Expr KOMUTUU)
# Köseli PAranızler(Command Subsitituin) "İceridkeiki Komutu Calistiirp Sonuucu Cikarr" demeketir.
set toplam [expr $sayi1 + $sayi2]

# Artik Sonucu(30) Dğru Basar!
puts "Toplam Sayi Sesi : $toplam"


#-----------------------------------------------------------
# 4. Asil Sihir: EXPECT (Cihazlarla Konusn Robot) Kutuphnesi!
# Bir Lİnux SunuCuya (Ya dA Router'a CihAzninaa) Sifresiini Girripp Baglkalanan OtomasyoN:

spawn ssh root@10.0.0.5     ;# SSH Cihazna İsaeGini FIRLAT VEE Basltt!

# Cihazin Ekrana(Termialne) "password" Yazmaiisini BEEKLE(Expect)!!
expect "password:"          

# Paswpord Yazssini Gorfugun Andaa Sifreyi(123445) Yollla ve \r(Enter TusunNaB Bas!)
send "123456\r"             

# Baglatni Kurludkta Sira Cihadxda Rourt Ayarloaninni(Ifconfig Vss) Ypaaavs cikK!
expect "#"                  ;# Root Sİmgeesni (Bashnladngincii BEkje)
send "ifconfig eth0 down\r" ;# SunuCunuN İNtnernerinnI KESS(Hakckler Yda DevOps otmoasunu)!
```

`expect` Eklentisi o kadar Güçlüdrür Ki; TCL adını Unutturmuş, Hackerlerın Veya Veritabanını Kuruculaın "Şifre Sorma Ekranlar"nda (Interactive Prompts) İnsanin Yapanaciğı İsleri Bİlgisayarin Klavyeyi Ele GEcreereK kendieeninden Yzaamasinini Sąğalayan Yüce Bior Güç Olusmuşstur.

## Kimler Kullanır?
* Evrenndeki **Network / Ağ Sİstem Uzmanları**, Sysadmine'ler VE IoT (Router/Modem) Cihazı Geliştiricileri.
* Çip(Motherboard/İşlemcİ) Tasarımcıları Olan **Donanım (Hardware/EDA) Mühendisleri**. Silikon vadsndind Çip tasarmlarini "Build/Compile" Ederken Cmake Yerine Tcl Scripiylye Cİhezi Atesleyeeller. Yaşlı ama asla ölmeyen , Çok Spedsifi Bir Kabuk dilidriir.
