# TCL (Tool Command Language)

## Özet
TCL (Tickle diye okunur); 1988 yılında John Ousterhout tarafından icat edilen, "Her şey ama İSTİSNASIZ her şey bir Karakter Dizisidir (String)" felsefesiyle yaratılmış, Özellikle Elektronik Tasarım Otomasyonları (EDA - Çip üretim yazılımları) ile Cisco Router/Ağ cihazlarının iç yapılarında komut tetikleyici (Gömülü Scripting) olarak kullanılan, çok tuhaf ve eşi benzeri olmayan bir **Evrensel Komut Dizisi (Script)** dilidir.

## Nedir ve Ne İşe Yarar?
1980'lerde eğer siz "AutoCAD" ya da bir "Çip (CPU) Çizim Programı" yazdıysanız; Müşterilerinize programın içinde kullanabilmeleri için bir Makro/Betik (Script) dili sunmak zorundaydınız.
O dönemde Python popüler değildi. Mühendisler her C programı için uyduruk, yarım yamalak betik dilleri yazıp duruyordu.

John Ousterhout dediki: **Öyle bir Dil(TCL) yapacağım ki; Gidip bunu Saniyeler İçinde kendi C/C++ programınızın İÇİNE GÖMEBİLECEKSİNİZ! (Embeddable). Üstelik Bu dilin Matematikten(Integer) falan haberi olmayacak. Koddaki her şey Düz Yazı(String) olacak!**

**Ne İşe Yarar?**
* **Elektronik Çip Tasarımı (EDA):** Bugün Synopsys, Cadence veya Mentor Graphics gibi milyar dolarlık İşlemci (Çip/Devre) çizim programlarında, donanım mühendislerinin "Bu 10.000 adet transistörü Rastgele Şuraya Kopyala/Test Et" dediği makro komutları İstisnasız TCL diliyle yazılır.
* **Ağ (Network) Cihazları:** Cisco marka Switch veya IP Telefonlarına uzaktan Makro/Ayar yollarken, Cihazın içindeki kabuk (Shell) `tcl` komutlarını okur. `expect` isimli efsanevi eklentisi sayesinde SSH veya Telnet üzerinden Karşı Cihazı "Bekleyip-Yanıt Mekanizmasıyla" Tıpkı Bir İnsanmış Gibi Otonom Haklar (Otomatikleştirir).

## Dilin Mantığı ve Kod Yapısı
Tamamen **Komut (Command)** ve **Argüman (Strings)** odaklıdır.
Eğer siz TCL'de `set a 5` yazarsanız, TCL `5` sayısını Rakam (Integer) olarak algılamaz. Onu `5` karakteri (String) olarak algılar. Matematik Yapmak isterseniz `expr` (Expression - Matematik Yap) komutunu Çağırmak Zorundasınız!

### Örnek Bir TCL Kodu: Otomasyon ve Bekleme Makrosu (Networkçülerin Gözdesi)
Basit Bir Değişken Tanımlaması ve Matematik Çilesi (Her şeyin String olmasından Kaynaklanan Tuhaflık):

```tcl
# BU BİR TCL KODUDUR (Yorum Satırı Diyez'dir)

# 1. DEĞİŞKEN ATAMA (DİKKAT: "=" eşittir sembolü yoktur! Komut olarak yazılır!)
set sayi1 10
set sayi2 20

# 2. DEĞİŞKENİ ÇAĞIRMA (Dolar İşaretiyledir!)
# Ekranda 1020 çıkar! Çünkü Bunlar Metin (String) dir, Yanyana Yapıştırılır!!
puts "$sayi1$sayi2" 

# 3. MATEMATİĞE ÇEVİRME (Expr KOMUTU)
# Köşeli Parantezler (Command Substitution) "İçerideki Komutu Çalıştırıp Sonucu Çıkar" demektir.
set toplam [expr $sayi1 + $sayi2]

# Artık Sonucu (30) Doğru Basar!
puts "Toplam Sayı Sesi : $toplam"


#-----------------------------------------------------------
# 4. Asıl Sihir: EXPECT (Cihazlarla Konuşan Robot) Kütüphanesi!
# Bir Linux Sunucuya (Ya da Router/Cihazına) Şifresini Girip Bağlanan Otomasyon:

spawn ssh root@10.0.0.5     ;# SSH Cihazına İsteğini FIRLAT VE Başlat!

# Cihazın Ekranına (Terminaline) "password" Yazmasını BEKLE (Expect)!!
expect "password:"          

# Password Yazısını Gördüğün Anda Şifreyi (123456) Yolla ve \r (Enter Tuşuna Bas!)
send "123456\r"             

# Bağlantı Kurulduğunda Sıra Cihazda Router Ayarlarını (Ifconfig vs) Yapıp Çık!
expect "#"                  ;# Root Simgesini (Başlangıcı Bekle)
send "ifconfig eth0 down\r" ;# Sunucunun İnternetini KES (Hacker ya da DevOps otomasyonu)!
```

`expect` Eklentisi o kadar Güçlüdür Ki; TCL adını Unutturmuş, Hackerların Veya Veritabanı Kurucularının "Şifre Sorma Ekranları"nda (Interactive Prompts) İnsanın Yapacağı İşleri Bilgisayarın Klavyeyi Ele Geçirerek Kendiliğinden Yazmasını Sağlayan Yüce Bir Güç Oluşturmuştur.

## Kimler Kullanır?
* Evrendeki **Network / Ağ Sistem Uzmanları**, Sysadmin'ler VE IoT (Router/Modem) Cihazı Geliştiricileri.
* Çip (Anakart/İşlemci) Tasarımcıları Olan **Donanım (Hardware/EDA) Mühendisleri**. Silikon Vadisinde Çip tasarımlarını "Build/Compile" Ederken CMake yerine Tcl Scriptiyle Cihazı Ateşlerler. Yaşlı ama asla ölmeyen, Çok Spesifik Bir Kabuk dilidir.
