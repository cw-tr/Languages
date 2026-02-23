# Regex (Regular Expressions)

## Özet
Regex (Kısa adıyla, Düzenli İfadeler); 1950'lerde Amerikalı matematikçi Stephen Cole Kleene tarafından formal dil teorisinde modellenen ve sonrasında Unix / Perl komut satırlarında vücut bulan; **Programlama Dininde "Metin Arama, İçerisinden kelime Çıkarma, Veri Doğrulama(Validation) Karadeliği"** olarak bilinen, Yazı dizisi (String) içinde Kuralsal/Sihirli Desenler (Pattern) arayıp yakalamaya yarayan evrensel "Arama/Kural/Filtre" sözdizimidir.

(*Bir Programlama Dili veya Markup Dili değildir. Hemen Tümü Programlama Dillerinin: Java, C#, JS, Python; içine "Gömülü olarak (RegEx Engine)" bulunan Kutsal Metin Madenciliği Büyüsüdür.*)

## Nedir ve Ne İşe Yarar?
Eskiden (ve bir Çok Acemi kodcunun Yaptığı Hata); Bir Kullanıcının Girdiği E-Posta adresinin Gerçek Orjinal Bir Eposta mı Olduğunu C dilinde Doğrulamak (Validate Etmek) şuydu:
"İçinde `@` İşareti Var mı Diye Bak... Varsa Ardından `.com` harflerini Ara... Yok O Varsa Uzunluğunu ölç". Bu hem 50 satırlık İğrenç Bir Spagetti Kod Doğururdu, hem de `ali@@.com..com` Giren adamı O Kodla Yakalayamazdınız Sistemi Patlatıp Sızardı.

Regex Dedi ki: **"Sadece 10 Karakterlik, Matematiğin Sembollerinden Bir Desen(Kurallar Şablonu/Pattern) Yaz ve Motor O String'in Bütün Harflerini Göz Kırpmasında Tarayıp Senin Formülüne Kusursuz uyuyor mu Çıkarsın!"**

**Ne İşe Yarar?**
* **Veri Doğrulama (Validation):** Şifreniz (En az 1 Büyük, 1 Küçük, Sayı ve Özel Karakter içermeli) Kuralı Sadece Regex Makinesiyle "Form Kuralına" Yazılır!
* **Kazı / Sıyırma (Scraping):** 10 Bin sayfalık HTML / Web sitesi İndiriniz. Sisteminizden Sadece "05XX ile Başlayan Telefon Numaralarını" Çekip Ayıklamak İstiyorsanız; Yüzlerce (Split) Bölümü yazmak yerine 1 Satırlık Regex yazarsınız, Pıt diye O Datanın İçinden Milyonlarca Telefon verisini Önünüze Tabağa Dizer. Karadelik gibi Süzücü ve Sihirlidir.

## Dilin Mantığı ve Kod Yapısı
Evrenin (Ezoterik diller hariç) yazması ve Okuması "EN KORKUNÇ VE BEYİN YAKICI" ama en kısa olan şifrelemesi ve Matematiği Regex'tir.
Aralarında Özel Görev (Meta Karakterler) Alan harfler:

1. `^` : Satır Şununla BAŞLASIN Emiri.
2. `$` : Satır Aynı Şekilde BİTSİN.
3. `.` : Herhangi ve Her Çeşit Ufak 1 Karakter! (Nokta her şeydir).
4. `*` : Kendinden Önceki (Örn: A*) Kuralından "Sıfır ya da Binlerce, Sınırsız var!" demektir. (Asterisk).
5. `+` : Karakterden En az 1 Tane OLMAK ZORUNDA, (Eksik olamaz Sonsuz olabilir).
6. `\d` : (Digit/Sayı) : Gelen Şey Sayı (0-9) olmak zorundadır. Harf gelemez.
7. `[a-zA-Z]` : Sadece Tüm Küçük ve Büyük İngilizce(Latin) Alfabe harfi! Başka Şey Yok!
8. `?` : Opsiyonel! "Oladabilir, Olmayadabilir (0 veya 1 Tane)". 

### Örnek Bir Regex Mimarisi: (Email Adresinin Kusursuz Matematiksel Modeli) 
Sıradan gibi Görünen O E-Posta şablonunu (user@domain.com) ; Dünyadaki En Güvenli, C#, Java, Python dillerinde 1 Satırda Adamın girdiği epostayı Hacklenemez Kılan Doğrulama Formülü Şudur (`/ /` Kapanları arasına yazılır):

```regex
/* HATA YAPILAMAZ MUKEMMEL E-POSTA(MAIL) DOGRULAMA REGEX KODU */

^[\w\.-]+@[a-zA-Z\d\.-]+\.[a-zA-Z]{2,6}$

```

**Bu Deli-Saçması Kodun (Tek Tek İnsan Diline Çevrilmiş) Harf Harf Okunuşu Anlamı:**
* `^` ==> Kuralı En baştan İtibaren Katıla!
* `[\w\.-]+` ==> (Kullanıcı Adı Kısmı): Sadece Kelime karakterleri(\w), Yada Nokta, Veya Çizgi OLABİLİR Ve Bunlardan EN AZ 1 TANE OLMAK ZORUNDADIR(Kilit). (Yani bos olamaz). 
* `@` ==> Cuk Diye At(Kuyruklu A) Oturmak Zorunda!
* `[a-zA-Z\d\.-]+` ==> (Domain Kısmı: gmail, hotmail gibi): Sadece Harf, Rakam, Çizgi olabilir ve Yine en az 1 karakter olmak Zorunlu!
* `\.` ==> Uzantı Başlıyor Kaçış (Yani Gerçek Özel bir (.) Nokta geldi!). (Özel Karakterin Özelliğini Kaçırmak için Slash\)
* `[a-zA-Z]{2,6}` ==> Uzantı (.com, .tr, .co.uk): SADECE Harf olmak Zorundadır ve MİNİMUM 2 HARF; MAKSİMUM 6 HARF Uzunluğunda (.online vb) kilitli Limitlidir! Uzun olursa Kabul Etme.
* `$` ==> Satır Mutlaka  Burada Son bulsun. Arkasına Çöplük kod enjeksiyonu yasaktır.

Evet! Adamların yazdığı 500 satırlık Python Kötü(Validation) Kodu; Programlanın Has Çekirdeği Olan REGEX ile "20 Karakterlik Matematiğe" Gömülüp Ezip Geçer. Okuması imkansız görünür, Yazması Sanattır!

## Kimler Kullanır?
* Evrendeki bütün **Kaliteli Koder/Mühendis, SysAdmin Kankalar ve Güvenlik İstihbaratı** Çeker. Sistemlerde (ElasticSearch), Log ve Linux Sunucularda Textlerin içerisinde (Grep arayışında vs) hata bulmak Okyanustaki Damlayı Kriptografi Regex ile Sökmektir. Herkes Nefret ve Eziyet eder Lakin Hayat Kurtaran Zırhtır. Şifreyi(Pasword Policy) belirleyen Regex Tılsımıdır.
