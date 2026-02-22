# Awk

## Özet
AWK; 1977 yılında Bell Laboratuvarlarında **A**ho, **W**einberger ve **K**ernighan (Kelimelerinin baş harfleri) tarafından icat edilen, Unix/Linux komut satırlarının efsanevi Text (Metin) satırlarını virgüllerine/boşluklarına göre bıçak gibi saniyede "Desen Eşleştirip Kesmeye" yarayan, evrendeki ilk tam-teşekküllü küçük veri madencisi Betik (Script) dili komutudur.

## Nedir ve Ne İşe Yarar?
1970'lerde terminal penceresinde (Linux Terminali) bir klasörün listesine baktığınızda veya milyon satırlık bir Telefon Rehberi (Yankı) logunda; "Ben sadece Dosya Boyutu 5 MB'den büyük olan satırların SADECE 3. SÜTUNUNU kopyalamak istiyorum!" dediğiniz an... Bu iş C'de günlerce, Bash ortamında zorlu grep süreçleri alırdı.

AWK; veriyi (Metni) bir excel tablosu gibi "Satır (Record)" ve "Sütun (Field)" lara otomatik olarak bölme Felsefesi üzerine kurulmuştur. Siz ona bir komut (Script) ve boşluklu metin atarsınız, Awk metni havada böler ve "Şu koşulu sağlıyorsa bu işlemi yap" diyerek muazzam bir akış (Stream) veri analizcisi gibi çıktıyı size verir.

**Ne İşe Yarar?**
* **Satır (Sütun) Parçalayıcı Veri İşleme:** Linux'da `/etc/passwd` (Sistem kullanıcı kayıtları - İki nokta üst üste ayrılmış veri) dosyalarındaki Milyonlarca satırdan, SADECE gerçek kullanıcı isimlerini veya İki kolonu çarpıp toplamlarını hesaplamak (CSV dosyaları) için Terminal'de en çok kullanılan 1 Satır (One-Liner) Ninja Komutudur.
* Log Dosyası Crahslarini anında özetle, grep/sed'in çok zayıf (Algoritmik Matematik yapamayan) kaldığı yerde AWK ile "Kolondaki sayı 100'ü aşıyorsa C dili benzeri İf-Else" ile matematiğe sığın.

## Dilin Mantığı ve Kod Yapısı
Awk aslında C Syntaxı andıran (Kıvrırcık Parantezli `{}`) minik bir dildir. Temel felsefesi Olay-Sürücülü (Data-Driven / Event) kurgudur: 
**`KOSUL_ve_DESEN { YAPILACAK_EYLEMLER }`** döngüsü üzerine yatar.
Awk arkada SİZ GÖRMEDEN bir For döngüsü açar ve Satırları (Record) tek tek okur.

En muazzam sihiri **Dolar ($) Sembolleridir**. PHP'den farklı olarak Dolar, değişken DEĞİL; **Sütun numarasıdır**!
- `$0` : Cümleinin/Satırın Kendisi (Tamamı).
- `$1` : İlk Boşluğa (Veya Virgüle) kadar olan 1. Kelime/Sütun.
- `$3` : 3. Sütun (Kolon).

**Örnek İşleyiş (Sembolik Olarak):**
Elinizde şöyle bir metin var: *Ahmet 45 Istanbul*
Sadece maaşları toplamak ve basmak için Bash komutuna tek ok: `awk '{ print $2 }' txtdosyasi`
Siz ona For/While demezsiniz. O tüm satırları ezer geçer ve "$2 (Yani 2. sütundaki - 45)" sayiların tamamını basar.

### Örnek Bir Awk Kodu: CSV Log Tablosundan Matematik (One-Liner / Script Blokları)
Grep'in matematik yapamadığı; sadece Linux terminal komut satırına tırnak içine fırlatılan efsanevi Kısa-Syntax! Diyelim ki elimizde satıcılar var `("İsim,SatisMiktari")`

```awk
# AWK genellikle Linux Terminaline(Bash) Tek Satir Olarak firlatilir. 
# ÖRN Bash: cat maaslar.csv | awk -F ',' '{ if($2>100) print $1 }' 
# Ama BUYUK projeler icin AWK bir dil dosyasina (.awk) da yazilabilir. '# Yorum Satirlari boyledir'

# 1. BEGIN BLOGU: Hicbir dosyayi Okumadan ONCE (Sadece 1 Kere) Calisir!
# Excel'in Başlık Sutunu basma (veya Değişken Tanimlama) Bölgesi:
BEGIN {
    
    # AWK'ya Eziyet etme; FS (Field Separator - Sütun Ayirici) yi Virgül (,) Yap!
    FS = ","  
    print "--------- ŞİRKET PRİM RAPORU ----------"
    
    Toplam_Satis_Ucreti = 0
}

# 2. ANA KALP DONGUSU (Hic isim (Begin/End) verilmemis olan Blok) 
# Dosyada kaş satır varsa Sistem Bu Kıvrırcık() Blogun Icine Okadar GİRİP ÇIKAR (Gizli Array For):

# SADECE (Kosul / REGEX): İkinci sütunundaki miktar ($2) 50'den Büyük olan Satırlarda Çalış:
$2 > 50 { 
    
    # $1 (Birinci Sutun: Ahmet vb), İşle (Prim yuzde %10 hediye et), 
    PrimKazanilanTutar = $2 * 0.10
    
    # Ekrana Şak diye C dilindeki gibi formatli Output (printf) ver!
    printf "Personel: %-10s -> Ana Satis: $%d | Kazandigi Prim: $%.2f\n", $1, $2, PrimKazanilanTutar
    
    # Degisken (Global Tutar) surekli Matematiksel olarak(C formatinda += ) genisletilsin
    Toplam_Satis_Ucreti += $2
}

# 3. END BLOGU: Milyonlarca satır taranıp bittğinde (Ve For dongusu kırıldıgında)
# En son kapida cıkarken rapor/Sonuc basma yeri (Sadece Cikis aninda calsiır)!
END {
    print "---------------------------------------"
    print "TUM SISTEM TARANDI VE MILYON DOLAR TOPLANDI. "
    print "Sirketin Toplam Yaptigi Gecerli Ciro: $" Toplam_Satis_Ucreti
}
```
`awk -f kodumuz.awk magaza_verileri.csv` komutuyla Terminalden ateşlersiniz. Sed/Grep sadece String bulabilirken, Awk resmen İçerideyken `C` Programlamanın (Matematik/IF/While) omurgasını kullanıp satır satır veriyi parçalayan/manipüle eden devasa bir makinedir. 

## Kimler Kullanır?
* Evrendeki bütün **Sistem Yöneticileri, Bash (Kabuk) Programcıları ve Hacklerlar / Sızma Testi Uzmanları (Pentester)**. Çünkü Awk her Linux / Mac OS sisteminde gömülü (Built-in) gelir. Python yüklemeyeceğiniz sunucularda anında metinleri kırpar.
* Veri Bilimi uzmanlarının (Python/Pandas açmak zor geldiğinde) devasa 10 Gigabyte'lık bir veriyi ram şişmeden hızla bölüp CSV yapmak için başvurdukları "Süper Kılıçtır". Awk olmasaydı *Perl, Python ve Unix* borulama (Piping `|`) sisteminin çehresi çok zayıf kalırdı.
