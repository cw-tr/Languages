# Malbolge

## Özet
Malbolge; 1998 yılında Ben Olmstead tarafından, kelimenin tam anlamıyla **"Herhangi bir insanın bu dilde kod yazmasını İMKANSIZ KILMAK (Cehennem Eziyeti)"** amacıyla bilinçli olarak tasarlanmış, ismini Dante’nin *İlahi Komedya*’sındaki Cehennemin sekizinci katı olan "Malebolge"den alan, çalışırken kendi kodunu şifreleyen (Self-Modifying) gelmiş geçmiş en hastalıklı ezoterik programlama dilidir.

## Nedir ve Ne İşe Yarar?
Ben Olmstead diğer ezoterik dillerin (Örn: Brainfuck) hala "matematiksel bir mantığı ve hilesi" olduğunu fark etti ve "Sınırları o kadar zorlayalım ki, bunu bir insan kendi aklıyla yazamasın, kodu tahmin edemezsin" dedi. 
Malbolge'yi sadece **kötü ve karmaşık olmak üzere** icat etti. 

Öylesine imkansızdı ki, icat edildikten tam **İki Yıl Boyunca** evrendeki hiç kimse (Yaratıcısı dahil) ekrana sadece "Hello World!" basan bir kodu yazamadı! 2000 Yılında birileri Lisp dilinde "Arama Algoritması (Genetic Algorithm - Brute Force)" yapan yapay zeka tarzı bir robot yazdı da, Lisp botu Milyonlarca rastgele ihtimali hesaplayarak Malbolge'de o meşhur Hello World'u şans eseri çözüp basabildi. O gün teknoloji dünyası şok geçirmişti.

**Ne İşe Yarar?**
* **Psikolojik Bir Şaka:** Sadece bir programlama meydan okuması ve kriptografik (şifreleme) illüzyondur. 
* İnsanoğlunun mantık kapasitesinin bir donanıma karşı nerede kilitlendiğini ölçmek amaçlıdır. Bir işe yaramamakla kalmaz, işe yaraması BİLEREK ENGELLENMİŞTİR.

## Dilin Mantığı ve Kod Yapısı
Matematiği bilgisayarların Klasik İkili(Binary 0-1) sistemleri üstüne değil; **"Üçlü Sistem (Ternary 0-1-2)"** üzerine inşa edilmiştir. Değişkenler normal byte yerine 10-bitlik "Word" lerde tutulur.

İmkansızlığının Ana Sebepleri:
1. **Çılgın Komutlar Tablosu:** 8 komutu vardır ama bu komutların "Karakter karşılığı" sabit değildir! O anki Hafızadaki Adresin indeksine (Konumuna) bölünmesine vs göre şifrelenir. 
2. **Kendi Kendini Değiştirmesi (Self-Modifying):** Bir karakteri işlettiği Saniye... İşlettikten hemen sonra "O karakteri hafızadaki özel bir Kripto Tablosuna (Crazy Operation) sokup" kodu BAŞKA bir emire/işarete dönüştürür. Yani bir döngü yazarsanız, ikinci kez başa sardığında o döngünün kuralları o an bambaşka bir matematiğe dönüşmüştür!

**Örnek İşleyiş (Sembolik Olarak):**
Python: `print("H")`
Malbolge: Pointer "x" olsun, X hafızasına 62 sayısını ver. Ekran basma komutu o an "a" harfi mi? Bas. O anda a harfi "c" ye şifrelensin ve Hafıza 21'e aksın! (Kabus).

### Örnek Bir Malbolge Kodu: Ekrana Sadece "Hello World!" Basan O İlahi Robot-Çıktısı
İlk duyduğunuzda inanamayacağınız; bir insanın değil, başka bir makinenin milyonlarca varyasyonu tarayarak bulduğu ve çalıştırdığında ekrana net bir şekilde `Hello World!` basan o delilik satırı:

```malbolge
# Bu gördügünüz ascii cöplugu bozuk dosya degildir. Saf ve calisan bir Malbolge "Hello World"üdür:

(=<`#9]~6ZY32Vx/4Rs+0No-&Jk)"Fh}|Bcy?`=*z]Kw%oG4UUS0/@-ejc(:'8dc

# Ya da Malbolge 2000 yili revizyonunda bulunan daha "Stabil!" bir Hello World:
('&%:9]!~}|z2Vxwv-,POqponl$Hjig%eB@@>}=<M:9wv6WsU2T|nm-,jcL(I&%$#"
`CB]V?Tx<uVtT`Rpo3NlF.Jh++FdbCBA@?]!~|4XzyyR{-RVxwdHqo(!ib$&%eb#"
?C[M?!|\[WzU_QPO3=iE.Jh+mF[cBA@?[;~|4Xz>U{v-TVxwqmPqOnMle+J;%<H
```

Program çalışırken; ilk satırdaki `(` okuğunda (Hesaplaması 40 falan eder) onu ekrana birşey basmaz atlar. Sonra şifreleme motoru `(` işaretini alır, çılgın bir işlemden geçirip kodu o anlık `A` ya dönüştürür. U harfine falan geldiğinde bir anda hafızada ekrana "H" basılacak rakam denk gelir ve ekran "H" harfini çeker. 

Yazılımda Determinizm (Aynı Girdinin Aynı Çıktıyı vermesi)'ne en büyük başkaldırıdır, zira kod sabit kalsa da arka plandaki saatler durmaksızın rotasyon halindedir.

## Kimler Kullanır?
* **HİÇ KİMSE.** Bu dilde çalışan bir web sitesi veya hesap makinesi yapmak, C ile bir aya seyahat roketi yapmaktan (Kelimenin tam anlamıyla olasılıklar dahilinde) daha trilyonlarca kat daha uzun yıllar alır (Belki de evren simülasyonu süresi bitene kadar).
* Güvenlik Kriptografı delileri ve Reverse-Engineering (Tersine Mühendislik) yapan Siber Güvenlik Korsanları (Beyin limitlerini yakmak istediklerinde) bu cehenneme bakarlar. Ezoteriğin de Ezoteriği; Tapınak Şövalyesidir.
