# INTERCAL

## Özet
INTERCAL (Compiler Language With No Pronounceable Acronym); 1972 yılında Don Woods ve James Lyon (Princeton Üniversitesi öğrencileri) tarafından dönemin ciddi/sıkıcı dillerini (Fortran, COBOL) tiye almak (dalga geçmek) için yaratılan, **Dünyanın İLK Ezoterik (Parodi) Programlama Dilidir**. Kodların çalışabilmesi için sisteme aşırı derecede nazik davranmanız ve **"LÜTFEN (PLEASE)"** demeniz gereken manyakça bir mizah projesidir.

## Nedir ve Ne İşe Yarar?
1970'lerde her bilgisayar dili "Oku(Read)", "Yaz(Write)", "Eğer(If)" gibi ciddi askeri komutlar veriyordu. Woods ve Lyon "Öyle bir dil yapalım ki, diğer tüm dillerle alay etsin ve okunurluğu bir spagetti kasesinden farksız olsun" dediler. 

Kasten, yazımındaki her mantık diğer programcılara acı çektirmek için "Ters" tasarlandı. 

**Ne İşe Yarar?**
* **Dil Tasarımı Hicvi (Satire):** Başka hiçbir ezoterik dil yokken (Brainfuck'tan bile 20 yıl önce) yaratılmış; komedi ve kodun birleştiği o Atadır.
* Geliştiriciye bir dili nasıl "Tasarlamamanız (Kullanıcı Düşmanlığı)" gerektiğini gösteren mükemmel bir derstir.

## Dilin Mantığı ve Kod Yapısı
Bu dilin mantığı; bildiğiniz tüm if/else ve döngü algoritmalarını "Nezaket ve Kaba Kuvvet" (Politeness Constraint) ile kısıtlar.

1. **"PLEASE" (Lütfen) Kısıtlaması:** Eğer INTERCAL'de kod yazarken sisteme `DO` deyip emrederseniz belli bir süre sonra dil SİZİ REDDEDER (Çalışmayı Durur). Kodların arasına rastgele (Her komutun önüne değil, ama ortalama olarak her 5-6 komutta bir) **`PLEASE (Lutfen)`** yazmanız gerekir.
2. **Aşırı Nazik Olmak Suçtur:** Eğer aklınızı kullanıp "Her satıra PLEASE yazayım da işi garantiye alayım" derseniz... Compiler bunu anlar ve *"HATA: YOU ARE TOO POLITE (Aşırı Naziksiniz, Güven Vermiyorsunuz)"* diyerek programı Çökertir! (Lütfen oranının gizli ve dengeli bir matematiği/olasılığı vardır).
3. **COME FROM (Buradan Gel!):** Dünyadaki her kavgaya ve kötü koda sebep olan `GOTO (Oraya Git)` komutuyla dalga geçmek için INTERCAL `COME FROM`'u çıkarmıştır. Sİz bir satıra çalışsın diye kod yazmazsınız, O koda "Satır 14'ten Bana Gel!" dersiniz. Kod nerede tetikleneceğini bilmeden mayın gibi saklanır (Tersine Spagetti).

**Örnek İşleyiş:** Değişken Tipi veya sayı belirlerken Sembol C şovudur: `.`(Spot/Nokta), `:`(İki nokta).

### Örnek Bir INTERCAL Kodu
1970'li yılların deliliğinden, değişkenlerin birbiriyle dalga geçtiği (Ve lütfen demeden asla çalışmayan) bir sistem dizisi:

```intercal
DO ,1 <- #13
PLEASE DO ,1 SUB #1 <- #238
DO ,1 SUB #2 <- #108
DO ,1 SUB #3 <- #112
DO ,1 SUB #4 <- #0
DO ,1 SUB #5 <- #64
DO ,1 SUB #6 <- #194
DO ,1 SUB #7 <- #48
PLEASE DO ,1 SUB #8 <- #22
DO ,1 SUB #9 <- #248
DO ,1 SUB #10 <- #168
DO ,1 SUB #11 <- #24
DO ,1 SUB #12 <- #16
DO ,1 SUB #13 <- #162
PLEASE READ OUT ,1
PLEASE GIVE UP
```

Yukarıdaki kod, klasik Hello World! u türetmek adına Sayıları hafızalara (`DO ,1 <-`) ezer. Dikkat edilirse satırların bazısında Sadece `DO` yazarken (Emir verirken), program çökmesin (Trip Atmasın) diye aralara `PLEASE DO` (Lütfen yap) serpiştirilmiştir. 
Son olarak programdan Çıkış yapmak (Exit/Return 0) komutu yoktur. Programdan çıkmak için Sisteme **`PLEASE GIVE UP` (Lütfen Artık Pes Et!)** komutu verilmelidir ki Bellek rahatlasın.

## Kimler Kullanır?
* 1972 Yılındaki o devasa delikli kart bilgisayar jenerasyonunun asil ve sinirli profesörleri birbirleriyle dalga geçmek için kullanırdı.
* Günümüzde doğrudan kodu yazılabilecek kadar "Zevkli" veya Mantığı (Görseli) olan bir ezoterik dil değildir. Kasten kötüdür. Mızmız bir kod derleyicisinin "Yapay Zeka olmadan da nasıl İnsan Kaprisi yapabildiğini" anlatan muazzam bir Atadır. 
