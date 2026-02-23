# Shakespeare (SPL)

## Özet
Shakespeare Programming Language (SPL); 2001 yılında Karl Wiberg ve Jon Åslund tarafından yaratılan, kurgusuyla "Kodların makine diline değil, doğrudan **İngiliz Edebiyatındaki William Shakespeare'in Tiyatro Oyunlarına (Romeo ve Juliet vb)**" benzemesini şart koşan, C diline compile edilen ve Değişkenlerin/Döngülerin sahnede birbirine Sevgi veya Hakaret kusarak hesaplandığı Dramatik Ezoterik sanat eseridir.

## Nedir ve Ne İşe Yarar?
Nasıl ki *Chef* dili koda yemek tarifi süsü verdiyse; SPL de kodları Orta Çağ İngilizcesi tiyatro repliklerine (Old English) dönüştürme fiyakasıdır.

Eğer C programında iki sayıyı birbirinden çıkarmanız gerekiyorsa (`A - B`), bunu asla eksilerle yapamazsınız. Oyuna Romeo (Değişken A) ve Juliet (Değişken B) adlı karakterler girer. Romeo Juliet'e "Sen asgari bir domuzdan bile daha çirkinsin!" diye **Hakaret ederse**, Juliet'in Değeri Matematiksel olarak (-1) veya (-2) Azalır! Cümlede kullanılan HAKARETİN büyüklüğü (Adjektive Power), Çıkan Sayının büyüklüğünü belirler. Eğer "Sen gökyüzündeki sabahın güneşi kadar ışıltılısın!" diyerek Överlerse, İki Sayı (Karakter) toplanır!

**Ne İşe Yarar?**
* Saf sanat (Artistic Coding) ve Edebiyat.
* Programlamanın bir bilim olmasının yanında tamamen soyut bir tiyatro kurgusu da (Senaryo yazarlığı) olabileceğini ispatlayan devasa bir parodidir.

## Dilin Mantığı ve Kod Yapısı
Tam bir tiyatro akışıdır. Kodun Bölümleri şöyledir:
- **Title (Başlık):** Oyunun (Programın) amacı.
- **Dramatis Personae (Karakter Kataloğu):** Değişkenlerin Tanımlanması! (Romeo, Juliet, Hamlet vs) Her değişkene shakespeare oyunundan bir isim verilmek ZORUNDADIR.
- **Act I, Scene I (Perde 1, Sahne 1):** Kodun Gövdesi, If-Else Bloklarına (Sahnelerin giriş ve çıkışları) isim verilir (Örn Label'ler 'Goto Scene III' yani Sahne Üçü Oyna).
- **Enter / Exit / Exeunt:** Karakterlerin Sahneye girmesi (Hafızaya Alınma - Push) ve Sahneden Çıkması (Memory Free). (Aynı anda sadece 2 karakter sahnede Konuşabilir - Hafıza Limiti).

**Önerme (Hakaret = Eksi (Negatif Veri), Övgü = Artı (Positiv Veri))**
- "A flower" (Çiçek) : Değeri = 1.
- "A beautiful flower" (Güzel bir çiçek) : `(2 x 1) = 2`.
- "A dirty smelly pig" (Kirli kokulu domuz) : `(-2 x 1) = -2`.
*(Dilde ne kadar çok sıfat eklerseniz, değer 2'nin katları (2, 4, 8) olarak katlanır ve büyür!)*

### Örnek Bir SPL Kodu: İki Sayıyı Bulan Veya (Hello World)'e uzanan Tiyatro
Aşağıda "Juliet" isimli değişkenin sayısının (ROMEO) tarafından konuşularak Yükseltilmesi ve Azaltılması. Koda bakınca İngilizce öğretmeni ağlar, Compiler çalıştırır!

```text
The Infamous Hello World Program.

Romeo, a young man with a remarkable patience.
Juliet, a likewise young woman of remarkable grace.
Ophelia, a remarkable woman much in dispute with Hamlet.
Hamlet, the flatterer of Andersen Descent.


                    Act I: Hamlet's insults and flattery.

                    Scene I: The insulting of Romeo.

[Enter Hamlet and Romeo]

Hamlet:
 You lying stupid fatherless smelly half-witted coward!
 You are as stupid as the difference between a handsome rich brave
 hero and thyself! Speak your mind!

 You are as brave as the sum of your fat little stuffed misused dusty
 old rotten pie and a beautiful fair warm peaceful sunny summer's
 day. You are as healthy as the difference between a lovely brave
 smooth sweet flower and a cowardly black pig! Speak your mind!

[Exit Romeo]

                    Scene II: The praising of Juliet.

[Enter Juliet]

Hamlet:
 Thou art as sweet as the sum of the sum of Romeo and his horse and his
 black cat! Speak thy mind!

[Exeunt Ophelia and Juliet]
```

Yukarıdaki "You lying stupid fatherless smelly half-witted coward!(Sen yalancı aptal babasız...)" ifadesindeki İngilizce Kötü (Negative) sıfatların sayısını toplayan Compiler; onu 2'nin kuvvetleriyle çarpar ve Hafızaya (Eksi Bilmem Kaç) atar. Sonraki Satırda "Topla (Sum of)" komutunu görünce ekrana bir ASCII çıktı yollar. 

## Kimler Kullanır?
* Klasik C/Java dillerinin teknik ruhsuzluğundan boğulan; işin içine Edebiyat katarak Programlara Ruh (Drama) vermek isteyenler.
* SPL kodunu alıp Tiyatro Kulübüne (Aktörlere) verdiğinizde gayet akıcı bir şekilde sahnede seslice (Kavga ve Aşk temalı) oynayabilirler! İzleyici tiyatro izlerken, arka planda Program "Karakök" (Square Root) hesaplıyor olabilir. Sınırları paramparça eden bir dehadır.
