# Perl

## Özet
Perl (Practical Extraction and Report Language); 1987 yılında Larry Wall (bir dil bilimci ve Unix programcısı) tarafından icat edilen, metin işleme (Text Processing) kabiliyeti dünyadaki tüm dillerden daha üstün ve esnek olan, internetin ilk yıllarındaki Web (CGI) altyapısını tek başına ayakta tutan, hem inanılmaz derecede "Çirkin/Karmakarışık (Write-only)" koduyla bilinen hem de **Sistem Yöneticilerinin (SysAdmin) İsviçre Çakısı** lakaplı efsanevi betik (Scripting) dilidir.

## Nedir ve Ne İşe Yarar?
1980'lerin sonunda Unix işletim sistemlerinde sistem yöneticileri devasa Log (Kayıt) dosyalarının içinden hataları bulmak için awk, sed, grep ve shell scriptlerini çorba gibi birbirine bağlıyordu. Larry Wall, "Bütün bu eziyeti tek bir çatıya toplayan, C dili kadar güçlü ama düz yazı (metin) parçalamakta C'den milyon kat kolay bir araç yapalım" dedi. Perl doğdu. 

Perl'in asıl patlaması 1990'larda İnternet (WWW) gelince oldu. HTML Form doldurup "Göder"e basan kullanıcıların verisini Web Sunucusunda alıp (CGI Script) saniyede parse edebilen (RegEx - Düzenli İfadeler sayesinde) TEK DİL Perl'di. 90'larda Web demek Perl demekti (Amazon, IMDb gibi devlerin ilk sistemleri %100 Perl kokardı).

**Ne İşe Yarar?**
* **Devasa Metin (Log/Data) Manipülasyonu:** Eğer elinizde "Aralarına virgül atılmış 10 Gigabyte'lık bozuk bir müşteri telefon rehberi" varsa; Bunu Python veya Java ile temizlemek sizi ağlatır. Perl, "Düzenli İfadeleri (RegEx)" dilin tam kalbine kendi malı gibi entegre ettiği için Regex'le 2 satırda metni jilet gibi traşlayıp temizler.
* **Unix Sistem Yönetimi ve Otomasyon (DevOps'un Atası):** Sistemde şifresi 90 günü geçen kullanıcıları bul, maillerini regex ile ayır, sistemden uyar ve raporunu PDF at! Komutlarını C dili hızında birleştiren Betik kralıdır.

## Dilin Mantığı ve Kod Yapısı
Perl'in felsefesi TİMTOADY (There Is More Than One Way To Do It - **Bunu Yapmanın Birden Fazla Yolu Vardır**)'dir. Python'un "Sadece Tek ve Temiz yol olmalı" kuralının TAM TERSİDİR. Perl'de bir işi 10 farklı (ve hepsi birbirinden daha tuhaf/sembolik) şekilde yazabilirsiniz. Bu yüzden okunabilirliği bazen o kadar düşüktür ki Perl'e *"Yazılan Ama Okunamayan Dil (Write-Only Language)"* veya *"Klavyede uyuyakalmış kedinin çıkardığı ses"* denir.

Değişkenler Tiplerle değil, **Sembollerle (Sigils)** başlar:
- Düz Değişken (Scalar - Metin/Sayı): `$` Dolar işareti (örn: `$isim = "Ali"`)
- Liste (Katar - Array): `@` At işareti (örn: `@ogrenciler`)
- Sözlük (Harita/Hash): `%` Yüzde işareti (örn: `%notlar`)

**Örnek İşleyiş (Sembolik Olarak):**
Python Döngü: `for kelime in liste: print(kelime)`
Perl Mükemmelliği (Sembol Çorbası): `print "$_\n" foreach @liste;` (`$_` Perl'de "Mevcut olan şey (Default Variable)" anlamına gelen Özel ve ürkütücü bir karanlık semboldür!).

### Örnek Bir Perl Kodu: Regex ile Log Dosyası Katili 
Ekrana basiti göstermek yerine; 1995'lerde Sistem Adminlerinin C++'ı çöpe atıp Perl'e taptığı o muazzam Regex (Pattern Matching) entegrasyonu:

```perl
# Perl dilinde yorum satırları Diyez/Hashtag (#) ile baslar

# 1. SEMBOLLER VE DEĞİŞKENLER (SIGILS)
# Dolar ($) Tekillik, Et (@) Cogulluk belirtir:
my $sistem_adi = "Cyber-Sunucu-01";
my @log_satirlari = (
    "ERROR: Saat 10:00 - Disk Doldu",
    "INFO: User Ali giris yapti",
    "ERROR: Saat 10:15 - RAM Tespit Edilemedi!",
    "WARNING: CPU Sicakligi Yuksek"
);


# 2. DÖNGÜ VE O KARANLIK ÖZEL DEĞİŞKEN ($_)
print "\n--- $sistem_adi LOG ANALIZI (SADECE HATALAR) ---\n";

# Foreach dongusu, Dizi(@log_satirlari) icinde doner.
foreach my $satir (@log_satirlari) {
    
    # 3. REGULAR EXPRESSION (REGEX) MUCİZESİ (Dilin Kalbine Gomulü)
    # Bashdaki Grep veya C'deki Kutuphanelere gerek yok! 
    # =~ m/.../ (Match Operatoru) ile Metni saniyesinde Kes:
    
    if ($satir =~ m/^ERROR: (.*) - (.*)$/) {
        
        # Perl Cokusleri (Hafizada Asili Kalan Regex Sonuclari: $1 $2 $3)
        # 1. Parantezin Ici (Saat): $1 'e
        # 2. Parantezin Ici (Hatanin Kendisi) : $2 'ye OTOMATİK DÜŞER!
        my $saat = $1;
        my $hata_nedeni = $2;
        
        # Nokta (.) isareti ile String(Metin)'ler birbirine Birlesir (+ degil)!
        print "HATA BULUNDU! Koordinat Saat: " . $saat . " | Mevzu: " . $hata_nedeni . "\n";
    }
}


# 4. TEK SATIR (ONE LINER) FELSEFESI (TIMTOADY)
# Bir seyi uzatmak istemiyorsan, İf'i veya Loop'u Tersine Kullanabilirsin!
my $izinsiz_giris = 1;
print "SISTEMI UYAR: Izinsiz Giris Var!\n" if $izinsiz_giris == 1; 
# (Yukardaki: Sadece eger oyleyse print et demenin Ters Okunusu!)
```

Perl C kodunu aratmayan Hızı; Regex motorunun kalbine gömülü olmasıyla o kadar pratikti ki, CGI(Web) dünyasının tartışılmaz lideri olmuştu. (Fakat kodu okumak 6 ay sonra kendiniz yazsanız bile aşırı zordu).

## Kimler Kullanır?
* Klasik donanım ve Ağ altyapı **Sistem Yöneticileri (SysAdmins)** Unix/Linux sunucularda metinleri "sed/awk" dan daha temiz birleştirmek için Hala Perl yazarlar. (`.pl` dosyaları).
* Biyoinformatik (Bioinformatics) alanında, Milyarlarca Harflik DNA dizilimlerini (A-T-G-C stringlerini) Python'dan çok daha hızlı taradığı ve RegEx ile böldüğü için çok popüler kalmıştır.
* Günümüz WEB programcılığında PHP, Ruby ve Python'a karşı savaşı %100 kaybetmiştir. "Eski" ve bakımı zor (Legacy) kod olarak kabul edilir; fakat Regex dünyasına getirdiği o pratik mühür tüm modern diller tarafından arakalanmıştır.
